---
title: Org mode 生效功能 / 新增键位 / 使用度审计
date: 2026-10-29
env: Emacs 31.1 · Org 9.8.7 · macOS
data: assets/keyfreq-log (+ keyfreq-days 年度归档, keyfreq-log.keys)
---

# 一、先说结论

1. **你的 org 配置里，真正生效的比写下来的少很多。** 因为 `readme.org` / `package.org` 里大量代码块标了 `:tangle no`，只存在于文档、不进 `init.el` / `my-package.el`。`org-modules`、`org-todo-keywords`、`org-babel-load-languages`、`org-agenda-files` 四个关键项全部是**默认值**——你注释里写的 habit / crypt / inlinetask / NEXT-STARTED-WAITING 流程、20 种 babel 语言，一个都没启用。
2. **新增键位一共约 83 条（org-mode-map 25 + global 55 + 3 条 ESC/多级前缀），但今年以来只有 6 个被按过。** 其余不是"忘了"，就是**按不出来**——尤其 `H-`（Hyper）家族 28 条，keyfreq 里有记录以来**一次都没有**。
3. **你的 org 使用重心已经从"写"移到"读"。** 全时段 org-mode 里 43.2 万次按键（占全部按键 52%），今年 10 天只有 475 次，且 80% 是鼠标点击、RET、DEL、方向键。

# 二、数据口径（很重要，否则结论会偏）

| 期间 | 有数据的日子 | 按键数 | 说明 |
| --- | --- | --- | --- |
| 2022 | 全年 | 276,090 | 归档 `keyfreq-days/2022.log` |
| 2023 | 全年 | 554,080 | 归档 `keyfreq-days/2023.log` |
| 2024–2025 | **空** | 0 | 无任何归档 |
| 2026 | 10-18 ~ 10-29 | 1,966 | 只有 10 天；其中 org-mode 475 次 |

- `keyfreq-log` 是累计总账（832,136 次），"今年"= 总账 − 2022 − 2023 的差值。
- `keyfreq-log.keys` 是按下瞬间记录的**真实键序列**（fork 的新特性），只有 2026 年数据：1,591 次按键、162 条键序列。
- 所以判断"熟不熟"要分两把尺子：**全时段**（是不是从来没建立过习惯）+ **今年**（最近是否生疏）。只看今年会把"以前很熟、只是这十天没碰"误判成"不会用"。
- 另注：keyfreq 的 mode 归类会把在 `debugger-mode` 里执行的命令记到 debugger-mode 名下，所以下文统计**按命令跨 mode 合并**。

# 三、生效的内置 org 功能（实测值）

| 功能 | 实际状态 | 生效范围 / 怎么触发 |
| --- | --- | --- |
| `org-modules` | 默认 10 个 `ol-*`（bbdb bibtex docview eww gnus info irc mhe rmail w3m doi） | 链接类型；**habit / crypt / inlinetask / protocol / id 均未作为模块启用** |
| `org-babel-load-languages` | **只有 `(emacs-lisp . t)`** | `C-c C-c` 执行代码块：只有 elisp 块能跑，python/shell/sqlite 等不行；`C-c C-v t` 缠结仍可用（按需 require ob-*) |
| `org-todo-keywords` | 默认 `TODO / DONE` | `C-c C-t`；NEXT / STARTED / WAITING / SOMEDAY / CANCELLED 未生效 |
| `org-capture-templates` | `t n b d dt do` 六个模板 | `ESC M-c`；写入 `~/org-roam` 下 task.org / capture.org / diary.org |
| `org-agenda` | 命令可用，但 **`org-agenda-files = nil`** | `ESC M-RET` 打开的是**空议程**；无 `org-agenda-custom-commands` |
| `org-refile` | `org-refile-targets = nil` | `C-c C-w` 实际只能退化为当前文件 |
| `org-export` | ascii html icalendar latex odt | `C-c C-e` |
| `org-src` | 原生高亮、`current-window`、29 条语言→mode 映射 | `C-c e` / `s-l` / `H-l` 进出编辑 |
| `org-attach` | 内置可用，**未做任何配置** | `C-c C-a` |
| 折叠/结构 | `org-startup-folded showeverything`、`org-catch-invisible-edits smart`、`org-special-ctrl-a/e t`、`org-insert-heading-respect-content t` | TAB / `C-c C-,` / `C-c t t` |
| org-id | `org-id-get-create` 可用，`.org-id-locations` 存在 | `C-c n o` |
| org-habit / org-clock / org-table / org-protocol | 库能 require，但**没有启用或配置** | — |

## 三个"你以为有、其实没有"的坑

1. **`org-agenda-files = nil`** → `ESC M-RET` 打开空列表。这是全时段只用过 1 次 agenda 的直接原因之一。
2. **`org-todo-keywords` 是默认值** → 你的 `my/sparse-tree-with-tag-filter` 里用正则匹配 `NEXT|TODO|WAITING|STARTED`，但你根本没有 NEXT/WAITING/STARTED 这几个关键字，这个函数的匹配面比预期窄。
3. **`org-modules` 是默认值** → `org-habit`、`org-crypt`、`org-inlinetask`、`org-protocol` 都没挂上；`org-protocol` 不启用，浏览器/系统级的 org 捕获链接就接不进来。

# 四、生效的第三方插件

## 4.1 org 专属（挂在 `org-mode-hook` 或按需加载）

| 插件 | 生效方式 | 影响范围 | 新键位 |
| --- | --- | --- | --- |
| **org-modern** | `org-mode-hook` | 仅 org 缓冲区：视觉美化、`…` 折叠符、隐藏强调标记、`org-special-ctrl-a/e`、`org-tags-column 0`；GUI 下表格美化（终端关闭） | 无（纯显示） |
| **org-sticky-header** | `org-mode-hook` | 仅 org：header line 显示完整路径面包屑（`full`） | 无 |
| **org-auto-tangle** | `org-mode-hook` | 仅**带 `#+auto_tangle: t` 的文件**（你的 readme.org / package.org）：保存即缠结出 `init.el` / `site-lisp/my-package.el` | 无 |
| **org-roam** | `:defer 2` + `org-roam-db-autosync-mode` | `~/org-roam`（含 `.md`）；右侧 33% 侧窗显示 backlinks | `C-c n a A c f g h i j l o r s t T` |
| **org-roam-ui** | `:after org-roam` | 浏览器里看关系图，跟随当前节点 | `C-c n u` / `C-c n z` |
| **consult-org-roam** | `:after (org consult)` | 全局 | `C-c n b` / `C-c n F` / `C-c n l` / `C-c n s` |
| **org-similarity** | `:defer t` | 按需；**本次实测 `require` 失败**（子模块未就绪） | 无 |
| **org-mac-link** | 全局绑 `H-i H-i` | 抓取 macOS 前台 App 的链接；**`require` 失败**，键能按但可能报错 | `H-i H-i` |

## 4.2 通用插件，但在 org 里同样生效

| 插件 | 在 org 里的表现 | 键位 |
| --- | --- | --- |
| consult | `imenu` / `org-goto` / `consult-imenu` 全部 remap 成 `consult-org-heading`（按标题跳转） | 原键位不变（`C-c C-j` 等） |
| vertico + marginalia + orderless + vertico-posframe/multiform | 所有 minibuffer 补全（`C-c n f` 选节点、capture 选模板） | — |
| which-key-posframe | 前缀键提示 | — |
| embark | 对链接/标题/元素执行动作 | `C-;` |
| **goggles** | 高亮被改动过的行（org 里很显眼） | 无 |
| **pangu-spacing** | 启动 3 秒后 `global-pangu-spacing-mode`：中英文之间自动插空格 | 无（自动） |
| **sis** | **装了但没接配置**（`my-package.el` 里没有），org 里不生效 | — |
| symbol-overlay | 高亮同名 symbol | `M-i` / `M-I` / `M-n` / `M-p` |
| avy / ace-window / expand-region / multiple-cursors | 跳转、窗口、选区、多光标 | 见下表 H- 家族 |
| hippie-expand | **remap 了 `dabbrev-expand`**，`C-<tab>` / `H-<tab>` 也是它 | `M-/`、`C-<tab>` |
| helpful | remap 了 describe-* 全家 | `C-h f/v/k` 等 |
| vundo / diff-hl / olivetti / magit | 撤销树、行级 diff、专注写作、Git | `H-z`、`H-v H-v`、`H-m H-m` |
| keyfreq（自维护 fork） | 记录按键；`C-c k` 生成热力图 HTML | `C-c k` |
| keycast / repeat-help / free-keys | 状态栏显示当前命令、重复键提示、空键位查询 | — |

# 五、新增 key sequence 全表（附今年用量）

## 5.1 org-mode-map 内新增（25 条，只在 org 缓冲区有效）

| 键 | 命令 | 全时段 | 2026 | 判断 |
| --- | --- | ---: | ---: | --- |
| `C-c l` | org-store-link | 17 | 0 | 用过，已生疏 |
| `C-c n o` | org-id-get-create | 383 | 0 | 常用过 |
| `C-c t t` | org-insert-structure-template | 557 | 0 | 常用过（等价 `C-c C-,`） |
| `C-c H-t` | my/sparse-tree-with-tag-filter | 1 | 0 | 几乎没用过 |
| `C-c H-n` | my/org-show-current-heading-tidily | 219 | 0 | 常用过 |
| `C-c e` / `s-l` / `H-l` | org-edit-special | 551 | 0 | 常用过（三个键同名，冗余） |
| `s-j` | org-babel-next-src-block | 0 | 0 | **从没用过** |
| `s-k` | org-babel-previous-src-block | 0 | 0 | **从没用过** |
| `C-c n f` | org-roam-node-find | 2,484 | 0 | 以前主力 |
| `C-c n i` | org-roam-node-insert | 3,295 | 7 | 今年还在用 |
| `C-c n c` | org-roam-capture | 0 | 0 | **从没用过** |
| `C-c n j` | org-roam-dailies-capture-today | 0 | 0 | **从没用过** |
| `C-c n r` | org-roam-node-random | 31 | 0 | 很少 |
| `C-c n g` | org-roam-graph | 11 | 0 | 很少 |
| `C-c n l` | org-roam-buffer-toggle | 91 | 0 | 用过 |
| `C-c n a` / `C-c n A` | alias-add / alias-remove | 1,120 / 0 | 0 / 0 | 加过 1120 次，删过 0 次 |
| `C-c n t` / `C-c n T` | tag-add / tag-remove | 868 / 2 | 0 / 0 | 同上 |
| `C-c n s` | org-roam-db-sync | 178 | 1 | 用过 |
| `<remap> imenu/org-goto` | consult-org-heading | 391 | 0 | 用过 |

## 5.2 全局新增（28 条显式 + 27 条 remap）

**H-（Hyper）家族 —— 全部 0 次，一条都没按过：**

| 键 | 命令 |
| --- | --- |
| `H-c H-c` | pyim-convert-string-at-point（中英转换） |
| `H-c H-h / H-n / H-p` | mc/mark-all-like-this、mark-next、mark-previous |
| `H-c H-l / H-a / H-e` | mc/edit-lines、edit-beginnings、edit-ends |
| `H-j H-j` / `H-j 2` / `H-j H-k` | avy-goto-char / char-2 / line |
| `H-k H-k` / `H-k H-j` | parrot 同义词替换 上/下一个 |
| `H-m H-m` | magit-status |
| `H-w H-w` / `H-w H-t` | ace-window / 单窗口切换 |
| `H-v H-v` | olivetti-mode |
| `H-z` | vundo |
| `H-t H-t` | 中英翻译 |
| `H-i H-i` | org-mac-link-get-link |
| `H-/` | comment-current-line-dwim |
| `H-e` | toggle-input-method |
| `H-<tab>` | dabbrev-expand |

> 风险：`H-` 需要键盘层真的发出 Hyper 修饰键（Karabiner / 系统映射）。keyfreq 里 `H-` 前缀 **0 次**，而 `s-`（Cmd）前缀正常有记录。要么这一层根本没生效，要么你从没走这条路——两种都意味着这 28 条键是"死键"。

**其他全局新增：** `C-;` embark-act、`C-=` expand-region、`C-<tab>` hippie-expand、`C-s-j/C-s-k` 缓冲首尾、`s-/` diff-lisp、`C--` 由 `negative-argument` 改成 `er/contract-region`（谨慎：数字前缀会失效）。

**remap 家族（27 条）：** 主要是 consult 接管（`C-x b`→consult-buffer、`M-g g`→consult-goto-line、`C-x C-f`→find-file-at-point…）、helpful 接管 `C-h` 系列、`M-/`→hippie-expand、pyim 接管 `M-f/M-b`（按词移动变中文分词）。

## 5.3 org 全局键（不在 mode-map 里）

| 键 | 命令 | 全时段 | 2026 |
| --- | --- | ---: | ---: |
| `ESC M-RET` | org-agenda | 1 | 0 |
| `ESC M-c` | org-capture | 0 | 0 |
| `C-c n u` | org-roam-ui-open | 197 | 6 |
| `C-c n z` | org-roam-ui-node-zoom | 0 | 0 |
| `C-c n b` | consult-org-roam-backlinks | 14 | 0 |
| `C-c n F` | consult-org-roam-file-find | 7 | 7 |
| `C-c n s`（非 org buffer） | consult-org-roam-search | 6 | 0 |
| `C-c k` | keyfreq-html-v2 | — | 1 |

⚠️ **同键不同域**：`C-c n l` / `C-c n s` 在 org 缓冲区是 org-roam 的（buffer-toggle / db-sync），在别的 buffer 是 consult-org-roam 的（forward-links / search）。同一条键两种行为，容易记混。

# 六、按"熟悉度"分三档

## A. 从没建立过习惯（全时段 0 次）——建议直接决定"留还是删"

| 功能 | 键 | 说明 |
| --- | --- | --- |
| org-babel 块间跳转 | `s-j` / `s-k` | 0 次；且 babel 只启用了 elisp |
| org-roam 捕获 | `C-c n c` | 0 次（你以前用别的路径建笔记） |
| org-roam dailies | `C-c n j` | 0 次 |
| org-roam alias/tag 删除 | `C-c n A` / `C-c n T` | 0 / 2 次 |
| org-roam-ui 聚焦 | `C-c n z` | 0 次 |
| org-agenda | `ESC M-RET` | 全时段 1 次，且没有 agenda files |
| org-capture | `ESC M-c` | 0 次（但 `org-capture-finalize` 有 113 次，说明模板是在 roam 流程里走完的） |
| 日程/截止/归档/附件 | `C-c C-s` `C-c C-d` `C-c C-x C-s` `C-c C-a` | 3 / 0 / 0 / 0 次 |
| 执行代码块 | `C-c C-v e` | 0 次 |
| 全部 H-* | 28 条 | 0 次，疑似不可达 |
| 自定义稀疏树 | `C-c H-t` | 1 次 |

## B. 曾经是主力，今年一次没碰（生疏，不是不会）

`C-c n f`（2484）、`C-c n i`（3295，今年 7 次）、`C-c n a`（1120）、`C-c n t`（868）、`C-c t t`（557）、`C-c e`/`s-l`（551）、`C-c C-c`（403）、`C-c C-o`（406）、`C-c n o`（383）、`C-c C-l`（258）、`C-c H-n`（219）、`C-c n l`（91）、`H-i H-i`（81）、`C-c C-x C-v`（41）、`C-c C-v t`（169）。

## C. 今年真正在用的（6 条）

`C-c n F`（7）、`C-c n i`（7）、`C-c n u`（6）、`C-c n s`（1）、`C-c k`（1）、`org-fold-hide-block-toggle`（2，TAB）。

其余 2026 org 操作全是：鼠标点击 105+84+21、`RET` 99、`DEL` 58、方向键 46。

# 七、建议动作（按性价比排序）

1. **先修 `org-agenda-files`。** 不设它，`ESC M-RET` 永远是空壳；只要 `(setq org-agenda-files '("~/org-roam"))` 加上 2~3 条 custom command，agenda 才从"0 次"变成可用功能。
2. **要么启用 H- 层，要么删掉。** 0 次记录说明它现在是纯噪声，还占着 28 条认知带宽。启用前先确认 Hyper 真的发得出来（`C-h k` 按一次看 Emacs 收到什么）。
3. **决定 babel 策略。** 只想用 org 写文档 → 把 `s-j/s-k`、20 个 `ob-*`、`org-babel-load-languages` 那段彻底注释掉；想用 org 当笔记本 → 补齐 `(python . t) (shell . t) (sqlite . t)`。
4. **把 `:tangle no` 的意图和被注释的配置对齐。** 现在最大的认知负担来自"文档里写了一套、实际跑另一套"（todo 序列、modules、babel 三处）。要么恢复缠结，要么在文档里明确标注"未启用"。
5. **修子模块。** `org-similarity` / `org-mac-link` 的 `require` 失败，`H-i H-i` 按下去很可能报错。
6. **清掉重复键。** `org-edit-special` 有 3 个键（`C-c e`/`s-l`/`H-l`），`C-c C-,` 与 `C-c t t` 同命令，`C-c n l/s` 同键不同域——每个只需要留一个。

---

*检查方式：`emacs -Q` 与 `emacs -l init.el` 两次 batch 运行，分别 dump `org-mode-map`/`global-map` 三层键位做差集；变量取实测值。检查过程中触发过一次 org-roam 数据库增量同步（仅刷新缓存）。*
