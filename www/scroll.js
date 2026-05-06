document.addEventListener("DOMContentLoaded", function() {

  function initScrollytelling(containerId) {
    const sections = document.querySelectorAll(
      ".scroll-section"
    );

    console.log("Found sections:", sections.length);

    if (sections.length === 0) return;

    const observer = new IntersectionObserver(
      function(entries) {
        entries.forEach(function(entry) {
          if (entry.isIntersecting) {
            const step = entry.target.dataset.step;
            console.log("Step visible:", step,
                        "Container:", containerId);
            if (window.Shiny) {
              Shiny.setInputValue(
                containerId + "-scroll_step",
                parseInt(step),
                {priority: "event"}
              );
            }
          }
        });
      },
      { threshold: 0.5 }
    );

    sections.forEach(function(section) {
      observer.observe(section);
    });
  }

  initScrollytelling("act1");
  initScrollytelling("act2");
  initScrollytelling("act3");
});
