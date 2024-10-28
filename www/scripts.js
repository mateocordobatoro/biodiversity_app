function togglePanel() {
  var panel = document.getElementById('overlay_container');
  if (panel.classList.contains('minimized')) {
    panel.classList.remove('minimized');
    document.querySelector(`[onclick="togglePanel()"]`).innerHTML = 'Minimizar Panel';
  } else {
    panel.classList.add('minimized');
    document.querySelector(`[onclick="togglePanel()"]`).innerHTML = 'Ampliar Panel';
  }
}
