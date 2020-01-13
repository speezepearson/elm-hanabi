function initMain() {
    var flags = window.hanabiFlags;
    if (!flags.hasOwnProperty("stateServerRoot")) {
        console.error('no stateServerRoot given');
        return;
    }
    if (!flags.hasOwnProperty("appRoot")) {
        console.error('no appRoot given');
        return;
    }
    Elm.Main.init({
      node: document.getElementById('elm'),
      flags: flags,
    });
}

window.addEventListener('load', initMain);
