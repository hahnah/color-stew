const app = Elm.Main.init({
    node: document.getElementById('elm-node')
});

app.ports.copyString.subscribe((str) => {
    let tempDiv = document.createElement('div');
    let tempPre = document.createElement('pre');
    tempDiv.appendChild(tempPre).textContent = str;
    tempDiv.style.position = 'fixed';
    tempDiv.style.right = '200%';
    document.body.appendChild(tempDiv);
    document.getSelection().selectAllChildren(tempDiv)
    document.execCommand('copy');
    document.body.removeChild(tempDiv);
});