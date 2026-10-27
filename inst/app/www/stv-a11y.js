// Accessibility fixes for markup that bslib generates and the app cannot set
// directly. Found with axe-core; see docs/PLAN.md (decisions log).
(function () {
  function fixNavbar() {
    // The navbar list has role="tablist", which may only hold tabs: the mode
    // and theme switches move to a group after it, and the spacer goes
    var nav = document.getElementById("main_nav");
    if (nav && !document.querySelector(".stv-nav-controls")) {
      var controls = document.createElement("div");
      controls.className = "stv-nav-controls";
      nav.querySelectorAll(":scope > li.bslib-nav-item").forEach(function (li) {
        while (li.firstChild) controls.appendChild(li.firstChild);
        li.remove();
      });
      nav.querySelectorAll(":scope > .bslib-nav-spacer").forEach(function (el) { el.remove(); });
      nav.after(controls);
    }
    // The page content is the main landmark
    var main = document.querySelector("body > .container-fluid");
    if (main && !main.hasAttribute("role")) main.setAttribute("role", "main");
  }

  // A focusable separator needs a current value: the sidebar width in pixels
  function updateHandle(handle) {
    var sidebar = handle.closest(".bslib-sidebar-layout")?.querySelector(":scope > .sidebar");
    if (!sidebar) return;
    handle.setAttribute("aria-valuenow", Math.round(sidebar.getBoundingClientRect().width));
  }
  function fixHandles() {
    document.querySelectorAll(".bslib-sidebar-resize-handle").forEach(function (handle) {
      updateHandle(handle);
      if (handle.dataset.stvA11y) return;
      handle.dataset.stvA11y = "1";
      ["pointerup", "keyup"].forEach(function (type) {
        handle.addEventListener(type, function () { updateHandle(handle); });
      });
    });
  }

  function fixAll() {
    fixNavbar();
    fixHandles();
  }
  document.addEventListener("DOMContentLoaded", fixAll);
  window.addEventListener("load", fixAll);
  document.addEventListener("shiny:value", function () { setTimeout(fixHandles, 0); });
})();
