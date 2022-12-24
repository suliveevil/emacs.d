for (const node of document.querySelectorAll('p')) {
    const copy = document.createElement(node.nodeName);
    copy.textContent = node.textContent;
    node.parentElement.insertBefore(copy, node.nextElementSibling);
    node.setAttribute('translate', 'no');
}
