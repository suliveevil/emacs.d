#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""emacs-dep-graph.py — Emacs 插件依赖关系图谱提取器

graphify 的核心解析器只覆盖 .py/.js/.go 等语言，不含 Emacs Lisp。
本脚本沿用 graphify 的 graph.json schema（root/files/nodes/edges），
为 Emacs 配置补一套专用的依赖提取规则：

  1. declared : elpa/<pkg>/<pkg>-pkg.el 里的 (define-package ... REQUIRES)
  2. declared : lib/<pkg>/*.el 头部的 ;; Package-Requires:
  3. after    : site-lisp/my-package.el 里 use-package 的 :after / :requires
  4. require  : lib/<pkg>/*.el 里的 (require 'foo)（仅 lib 源码，避免噪声）

产物：
  assets/emacs-plugin-deps.json   graphify schema 图谱
  assets/emacs-plugin-deps.dot    Graphviz
  assets/emacs-plugin-deps.svg    Graphviz 渲染
  assets/emacs-plugin-deps.html   D3 力导向交互图
"""
from __future__ import annotations

import json
import os
import re
import subprocess
import sys
from collections import Counter, defaultdict
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
ELPA = ROOT / "elpa"
LIB = ROOT / "lib"
SITELISP = ROOT / "site-lisp"
OUT = ROOT / "assets"

# 平台节点：几乎所有包都依赖它，画进图里会形成无意义巨型枢纽
PLATFORM = {"emacs"}


# ============================================================
# 极简 s-expression 读取（只够读 define-package）
# ============================================================
def read_forms(text: str, start: int, limit: int = 4):
    """从 start 处读取顶层 form，返回 [(kind, raw), ...]。"""
    forms, i, n = [], start, len(text)
    while i < n and len(forms) < limit:
        c = text[i]
        if c in " \t\n\r\f":
            i += 1
            continue
        if c == ";":
            while i < n and text[i] != "\n":
                i += 1
            continue
        if c == "'":                      # quote，跳过标记
            i += 1
            continue
        if c == "(":
            depth, j = 0, i
            while j < n:
                ch = text[j]
                if ch == "(":
                    depth += 1
                elif ch == ")":
                    depth -= 1
                    if depth == 0:
                        break
                elif ch == '"':
                    j += 1
                    while j < n and text[j] != '"':
                        if text[j] == "\\":
                            j += 1
                        j += 1
                j += 1
            forms.append(("list", text[i:j + 1]))
            i = j + 1
        elif c == '"':
            j = i + 1
            while j < n and text[j] != '"':
                if text[j] == "\\":
                    j += 1
                j += 1
            forms.append(("str", text[i + 1:j]))
            i = j + 1
        else:
            j = i
            while j < n and text[j] not in " \t\n\r\f()\";'":
                j += 1
            forms.append(("atom", text[i:j]))
            i = j
    return forms


def parse_requires_list(raw: str):
    """从 '((dep "1.0") (dep2 "2.0")) 里抽出名字。

    ELPA 的 -pkg.el 里依赖名多为符号而非字符串：
        '((emacs "28.1") (magit-section "4.7"))
    所以名字部分要同时兼容符号与字符串两种写法。
    """
    out = []
    for m in re.finditer(r'\(\s*"?([A-Za-z0-9_.:+-]+)"?(?:\s+"[^"]*")?\s*\)', raw):
        out.append(m.group(1))
    return out


def parse_pkg_el(path: Path):
    """解析 <pkg>-pkg.el，返回 (name, version, [deps])。"""
    try:
        text = path.read_text(encoding="utf-8", errors="replace")
    except OSError:
        return None
    m = re.search(r"\(define-package\b", text)
    if not m:
        return None
    # 从 "(define-package" 的开括号之后开始读，否则会把整个 form 读成一个元素
    forms = read_forms(text, m.start() + 1, 5)
    if len(forms) < 3 or forms[1][0] != "str":
        return None
    name = forms[1][1]
    version = forms[2][1] if forms[2][0] == "str" else "?"
    deps = []
    for f in forms[3:]:
        if f[0] == "list":
            deps = parse_requires_list(f[1])
            break
    return name, version, deps


def parse_package_requires(el_files):
    """从 .el 头部 ;; Package-Requires: 抽依赖。"""
    for f in el_files:
        try:
            text = f.read_text(encoding="utf-8", errors="replace")
        except OSError:
            continue
        m = re.search(r"Package-Requires:\s*(\((?:[^()]|\([^()]*\))*\))", text)
        if m:
            return parse_requires_list(m.group(1)), f
    return [], None


def parse_require_calls(el_files):
    """从 .el 里抽 (require 'foo)。"""
    names = set()
    for f in el_files:
        try:
            text = f.read_text(encoding="utf-8", errors="replace")
        except OSError:
            continue
        for m in re.finditer(r"\(require\s+'([A-Za-z0-9_.:-]+)", text):
            names.add(m.group(1))
    return names


# ============================================================
# 1. elpa 已安装包
# ============================================================
def scan_elpa():
    pkgs = {}
    for pkg_el in sorted(ELPA.glob("*/*-pkg.el")):
        info = parse_pkg_el(pkg_el)
        if not info:
            continue
        name, version, deps = info
        if name in PLATFORM or name == "@":     # @ 是 elpa 的占位伪包
            continue
        pkgs[name] = {
            "name": name,
            "version": version,
            "dir": str(pkg_el.parent.relative_to(ROOT)),
            "declared": sorted(set(deps)),
            "source": "elpa",
        }
    return pkgs


# ============================================================
# 2. lib submodule
# ============================================================
def scan_lib():
    pkgs = {}
    for d in sorted(LIB.iterdir()):
        if not d.is_dir():
            continue
        el_files = sorted(d.rglob("*.el"))
        if not el_files:
            continue
        main = d / f"{d.name}.el"
        ordered = ([main] if main.exists() else []) + [f for f in el_files if f != main]
        deps, src = parse_package_requires(ordered)
        # require 只从主文件取，避免把整个子目录的噪声算进来
        main_file = [f for f in ordered if f == main] or ordered[:1]
        pkgs[d.name] = {
            "name": d.name,
            "version": "git",
            "dir": str(d.relative_to(ROOT)),
            "declared": sorted(set(deps)),
            "requires": sorted(parse_require_calls(main_file)),
            "source": "lib",
        }
    return pkgs


# ============================================================
# 3. 配置里的 use-package（谁真正被启用 + :after 顺序依赖）
# ============================================================
def scan_config():
    active, after_edges = {}, []
    src = SITELISP / "my-package.el"
    if not src.exists():
        return active, after_edges
    lines = src.read_text(encoding="utf-8", errors="replace").split("\n")
    cur = None
    for ln in lines:
        m = re.match(r"\(use-package\s+([A-Za-z0-9_./-]+)", ln)
        if m:
            cur = m.group(1)
            active.setdefault(cur, {"ensure": None, "after": []})
            continue
        if cur is None:
            continue
        if ":ensure t" in ln:
            active[cur]["ensure"] = "t"
        elif ":ensure nil" in ln:
            active[cur]["ensure"] = "nil"
        # :after foo / :after (foo bar)
        am = re.search(r":after\s+(?:\(([^)]*)\)|([A-Za-z0-9_.-]+))", ln)
        if am:
            targets = am.group(1).split() if am.group(1) else [am.group(2)]
            for t in targets:
                active[cur]["after"].append(t)
                after_edges.append((cur, t))
    return active, after_edges


# ============================================================
# 4. 建图
# ============================================================
def build_graph():
    elpa = scan_elpa()
    lib = scan_lib()
    active, after_edges = scan_config()

    nodes, edges = {}, []
    for name, info in elpa.items():
        nodes[name] = info
    for name, info in lib.items():
        if name in nodes:
            nodes[name]["source"] = "both"       # 同名冲突：elpa + lib 各一份
            nodes[name]["lib_dir"] = info["dir"]
            nodes[name]["lib_declared"] = info["declared"]
        else:
            nodes[name] = info

    known = set(nodes)
    for name, info in nodes.items():
        is_active = name in active
        info["active"] = is_active
        info["ensure"] = active.get(name, {}).get("ensure")
        info["config_after"] = active.get(name, {}).get("after", [])

        for dep in info.get("declared", []):
            if dep in PLATFORM:
                continue
            info.setdefault("_out", set()).add(dep)
            edges.append({"from": name, "to": dep, "type": "declared"})
        if info.get("source") in ("lib", "both"):
            for dep in info.get("requires", []):
                if dep in PLATFORM or dep == name:
                    continue
                # 自身子模块的 require（如 dsh-emacs -> dsh-emacs-command）不算插件间依赖
                if dep.startswith(name + "-") or name.startswith(dep + "-"):
                    continue
                info.setdefault("_out", set()).add(dep)
                edges.append({"from": name, "to": dep, "type": "require"})

    for a, b in after_edges:
        if b in PLATFORM:
            continue
        edges.append({"from": a, "to": b, "type": "after"})

    # 去重
    seen, uniq = set(), []
    for e in edges:
        k = (e["from"], e["to"], e["type"])
        if k in seen:
            continue
        seen.add(k)
        uniq.append(e)
    edges = uniq

    indeg, outdeg = Counter(), Counter()
    for e in edges:
        outdeg[e["from"]] += 1
        indeg[e["to"]] += 1

    # 补全只被依赖、自身未安装的节点（多为内置库）
    for e in edges:
        for side in ("from", "to"):
            p = e[side]
            if p not in nodes:
                nodes[p] = {"name": p, "version": "-", "dir": "",
                            "declared": [], "source": "builtin", "active": False}

    out_nodes = []
    for name in sorted(nodes):
        n = dict(nodes[name])
        n.pop("_out", None)
        n["in_degree"] = indeg.get(name, 0)
        n["out_degree"] = outdeg.get(name, 0)
        n["kind"] = ("active" if n.get("active") else
                     ("installed" if n.get("source") in ("elpa", "lib", "both") else "builtin"))
        out_nodes.append(n)

    return {
        "root": str(ROOT),
        "files": len(out_nodes),
        "total_lines": 0,
        "languages": {"elisp": len(out_nodes)},
        "functions_total": 0,
        "meta": {
            "elpa_count": len(elpa),
            "lib_count": len(lib),
            "active_count": len(active),
            "edge_count": len(edges),
        },
        "nodes": out_nodes,
        "edges": edges,
    }


# ============================================================
# 5. 导出
# ============================================================
DOT_COLORS = {
    "active":    ("#E1F5EE", "#0F6E56"),   # 已启用第三方
    "installed": ("#FAEEDA", "#854F0B"),   # 装了没启用
    "builtin":   ("#F1EFE8", "#5F5E5A"),   # 内置
}
EDGE_STYLE = {"declared": ('solid', "#185FA5"),
              "require":  ('dashed', "#7F77DD"),
              "after":    ('dotted', "#D85A30")}


def to_dot(graph):
    L = ["digraph emacs_plugins {",
         '  rankdir=LR;',
         '  node [shape=box, style="rounded,filled", fontname="Helvetica", fontsize=10];',
         '  edge [fontname="Helvetica", fontsize=8];']
    for n in graph["nodes"]:
        fill, stroke = DOT_COLORS[n["kind"]]
        label = n["name"]
        if n["kind"] == "active":
            label += "\\n(启用)"
        L.append(f'  "{n["name"]}" [fillcolor="{fill}", color="{stroke}", label="{label}"];')
    for e in graph["edges"]:
        style, color = EDGE_STYLE[e["type"]]
        L.append(f'  "{e["from"]}" -> "{e["to"]}" [style={style}, color="{color}"];')
    L.append("}")
    return "\n".join(L)


HTML = """<!DOCTYPE html>
<html lang="zh-CN">
<head>
<meta charset="utf-8">
<title>Emacs 插件依赖关系图谱</title>
<script src="https://cdn.jsdelivr.net/npm/d3@7/dist/d3.min.js"></script>
<style>
  * { box-sizing: border-box; }
  body { margin:0; font-family:-apple-system,BlinkMacSystemFont,"PingFang SC",Helvetica,sans-serif;
         background:#F7F6F3; color:#2C2C2A; }
  header { padding:16px 24px 8px; }
  h1 { font-size:15px; font-weight:500; margin:0 0 4px; }
  p.sub { font-size:12px; color:#5F5E5A; margin:0; }
  .bar { display:flex; gap:16px; flex-wrap:wrap; align-items:center;
         padding:8px 24px 12px; font-size:12px; }
  .legend { display:flex; gap:14px; flex-wrap:wrap; }
  .lg { display:flex; align-items:center; gap:5px; }
  .dot { width:11px; height:11px; border-radius:3px; border:1px solid; }
  input { padding:5px 9px; border:1px solid #D3D1C7; border-radius:6px;
          font-size:12px; width:180px; background:#fff; }
  #chart { background:#fff; border-top:1px solid #D3D1C7; }
  svg { display:block; width:100%; height:720px; }
  .node { cursor:pointer; }
  .node rect { stroke-width:1; }
  .link { fill:none; }
  .label { font-size:9px; pointer-events:none; fill:#2C2C2A; }
  #tip { position:absolute; background:#fff; border:1px solid #D3D1C7;
         border-radius:8px; padding:8px 10px; font-size:12px; pointer-events:none;
         display:none; max-width:280px; line-height:1.5; box-shadow:0 1px 4px rgba(0,0,0,.08); }
  table { border-collapse:collapse; font-size:12px; margin:0 24px 24px; }
  td, th { border-bottom:1px solid #E8E6DF; padding:5px 12px 5px 0; text-align:left; }
  th { color:#5F5E5A; font-weight:500; }
  h2 { font-size:13px; font-weight:500; margin:20px 24px 8px; }
</style>
</head>
<body>
<header>
  <h1>Emacs 插件依赖关系图谱</h1>
  <p class="sub">__SUB__</p>
</header>
<div class="bar">
  <div class="legend">
    <span class="lg"><span class="dot" style="background:#E1F5EE;border-color:#0F6E56"></span>已启用</span>
    <span class="lg"><span class="dot" style="background:#FAEEDA;border-color:#854F0B"></span>已装未启用</span>
    <span class="lg"><span class="dot" style="background:#F1EFE8;border-color:#5F5E5A"></span>内置库</span>
    <span class="lg"><span style="width:18px;height:0;border-top:2px solid #185FA5"></span>声明依赖</span>
    <span class="lg"><span style="width:18px;height:0;border-top:2px dashed #7F77DD"></span>require</span>
    <span class="lg"><span style="width:18px;height:0;border-top:2px dotted #D85A30"></span>:after 顺序</span>
  </div>
  <input id="q" placeholder="搜索包名，高亮其依赖">
</div>
<div id="chart"></div>
<div id="tip"></div>
<h2>被依赖最多的包（枢纽）</h2>
<table id="hub"></table>
<script>
const DATA = __DATA__;
const color = {active:["#E1F5EE","#0F6E56"], installed:["#FAEEDA","#854F0B"], builtin:["#F1EFE8","#5F5E5A"]};
const ecolor = {declared:"#185FA5", require:"#7F77DD", after:"#D85A30"};
const nodes = DATA.nodes.map(d => Object.create(d));
const index = new Map(nodes.map(d => [d.name, d]));
const links = DATA.edges.filter(e => index.has(e.from) && index.has(e.to))
                        .map(d => Object.create(d));

const W = document.getElementById("chart").clientWidth, H = 720;
const svg = d3.select("#chart").append("svg").attr("viewBox", [0,0,W,H]);
const g = svg.append("g");
svg.call(d3.zoom().scaleExtent([0.2, 4]).on("zoom", ev => g.attr("transform", ev.transform)));

const sim = d3.forceSimulation(nodes)
  .force("link", d3.forceLink(links).id(d => d.name).distance(60).strength(0.35))
  .force("charge", d3.forceManyBody().strength(-160))
  .force("center", d3.forceCenter(W/2, H/2))
  .force("collide", d3.forceCollide(d => 6 + Math.min(d.in_degree, 10) * 1.3));

const link = g.append("g").selectAll("line").data(links).join("line")
  .attr("class", d => "link t-" + d.type)
  .attr("stroke", d => ecolor[d.type] || "#888780")
  .attr("stroke-width", 1)
  .attr("stroke-opacity", 0.5)
  .attr("stroke-dasharray", d => d.type === "require" ? "4,3" : d.type === "after" ? "1,3" : null);

const node = g.append("g").selectAll("g").data(nodes).join("g").attr("class", "node");
node.append("circle")
  .attr("r", d => 4 + Math.min(d.in_degree, 12) * 0.9)
  .attr("fill", d => color[d.kind][0])
  .attr("stroke", d => color[d.kind][1]);
node.append("text").attr("class", "label")
  .attr("x", d => 6 + Math.min(d.in_degree, 12) * 0.9).attr("y", 3)
  .text(d => d.in_degree >= 3 || d.kind === "active" ? d.name : "");

const tip = d3.select("#tip");
node.on("mousemove", (ev, d) => {
    const deps = (d.declared || []).join(", ") || "无";
    const by = links.filter(l => l.to.name === d.name).map(l => l.from.name).join(", ") || "无";
    tip.style("display","block").style("left",(ev.pageX+14)+"px").style("top",(ev.pageY+10)+"px")
       .html("<b>" + d.name + "</b> <span style='color:#5F5E5A'>" + (d.version||"") + "</span><br>"
             + "来源: " + d.source + " | 状态: " + (d.active ? "已启用" : "未启用") + "<br>"
             + "依赖: " + deps + "<br>被依赖: " + by);
  }).on("mouseleave", () => tip.style("display","none"));

sim.on("tick", () => {
  link.attr("x1", d => d.source.x).attr("y1", d => d.source.y)
      .attr("x2", d => d.target.x).attr("y2", d => d.target.y);
  node.attr("transform", d => "translate(" + d.x + "," + d.y + ")");
});

document.getElementById("q").addEventListener("input", ev => {
  const q = ev.target.value.trim().toLowerCase();
  if (!q) {
    node.attr("opacity", 1); link.attr("stroke-opacity", 0.5); return;
  }
  const hit = index.get(q) || nodes.find(n => n.name.toLowerCase().includes(q));
  if (!hit) { node.attr("opacity", 0.15); link.attr("stroke-opacity", 0.05); return; }
  const near = new Set([hit.name]);
  links.forEach(l => {
    if (l.source.name === hit.name) near.add(l.target.name);
    if (l.target.name === hit.name) near.add(l.source.name);
  });
  node.attr("opacity", d => near.has(d.name) ? 1 : 0.12);
  link.attr("stroke-opacity", d => (d.source.name === hit.name || d.target.name === hit.name) ? 0.95 : 0.04);
});

const hubs = nodes.filter(n => n.kind !== "builtin" && n.in_degree > 0)
                  .sort((a,b) => b.in_degree - a.in_degree).slice(0, 15);
document.getElementById("hub").innerHTML =
  "<tr><th>包</th><th>被依赖次数</th><th>状态</th><th>依赖方</th></tr>" +
  hubs.map(h => {
    const by = links.filter(l => l.to.name === h.name).map(l => l.from.name).slice(0,6).join(", ");
    return "<tr><td>" + h.name + "</td><td>" + h.in_degree + "</td><td>" +
           (h.active ? "已启用" : "未启用") + "</td><td>" + by + "</td></tr>";
  }).join("");
</script>
</body>
</html>
"""


def main():
    graph = build_graph()
    m = graph["meta"]

    (OUT / "emacs-plugin-deps.json").write_text(
        json.dumps(graph, ensure_ascii=False, indent=2), encoding="utf-8")

    dot_path = OUT / "emacs-plugin-deps.dot"
    dot_path.write_text(to_dot(graph), encoding="utf-8")
    svg_path = OUT / "emacs-plugin-deps.svg"
    r = subprocess.run(["dot", "-Tsvg", str(dot_path), "-o", str(svg_path)],
                       capture_output=True, text=True)
    if r.returncode != 0:
        print("[warn] dot 渲染失败:", r.stderr.strip()[:200], file=sys.stderr)

    sub = (f"elpa {m['elpa_count']} 个包 · lib {m['lib_count']} 个 submodule · "
           f"启用 {m['active_count']} 个 · 依赖边 {m['edge_count']} 条")
    slim = {"nodes": [{"name": n["name"], "kind": n["kind"], "version": n.get("version", ""),
                       "source": n.get("source", ""), "active": n.get("active", False),
                       "declared": n.get("declared", []), "in_degree": n["in_degree"]}
                      for n in graph["nodes"]],
            "edges": graph["edges"]}
    (OUT / "emacs-plugin-deps.html").write_text(
        HTML.replace("__DATA__", json.dumps(slim, ensure_ascii=False))
            .replace("__SUB__", sub), encoding="utf-8")

    # 控制台摘要
    indeg = Counter(e["to"] for e in graph["edges"])
    outdeg = Counter(e["from"] for e in graph["edges"])
    kinds = Counter(n["kind"] for n in graph["nodes"])
    print(f"节点 {len(graph['nodes'])}  边 {len(graph['edges'])}")
    print(f"  已启用 {kinds['active']} / 已装未启用 {kinds['installed']} / 内置 {kinds['builtin']}")
    byname = {n["name"]: n for n in graph["nodes"]}
    print("\n── 枢纽包 TOP12（第三方，被依赖最多） ──")
    shown = 0
    for name, c in indeg.most_common():
        if byname[name]["kind"] == "builtin":
            continue
        print(f"  {c:>3}  {name:<22} [{byname[name]['kind']}] "
              f"依赖方: {', '.join(sorted(set(e['from'] for e in graph['edges'] if e['to'] == name))[:5])}")
        shown += 1
        if shown >= 12:
            break
    print("\n── 被依赖最多的内置库 TOP5 ──")
    shown = 0
    for name, c in indeg.most_common():
        if byname[name]["kind"] != "builtin":
            continue
        print(f"  {c:>3}  {name}")
        shown += 1
        if shown >= 5:
            break
    print("\n── 依赖最广的包 TOP8 ──")
    for name, c in outdeg.most_common(8):
        print(f"  {c:>3}  {name:<22} [{byname[name]['kind']}]")
    dup = [n["name"] for n in graph["nodes"] if n.get("source") == "both"]
    if dup:
        print(f"\n── elpa 与 lib 同名（版本冲突风险） {len(dup)} 个 ──\n  " + ", ".join(dup))
    orphans = [n["name"] for n in graph["nodes"]
               if n["kind"] == "installed" and indeg[n["name"]] == 0]
    print(f"\n── 已装但无人依赖且未启用 {len(orphans)} 个 ──\n  " + ", ".join(sorted(orphans)))
    missing = sorted(set(scan_config()[0]) - set(graph["nodes"] and
                    {n["name"] for n in graph["nodes"]}))
    if missing:
        print(f"\n── 配置里启用但磁盘上找不到 {len(missing)} 个 ──\n  " + ", ".join(missing))
    print(f"\n产物: {OUT}/emacs-plugin-deps.json / .dot / .svg / .html")
    return 0


if __name__ == "__main__":
    sys.exit(main())
