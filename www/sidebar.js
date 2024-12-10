const btn_menu = document.querySelector(".btn-menu");
      const side_bar = document.querySelector(".sidebar");

      btn_menu.addEventListener("click", function () {
        side_bar.classList.toggle("expand");
        changebtn();
      });

      function changebtn() {
        if (side_bar.classList.contains("expand")) {
          btn_menu.classList.replace("bx-menu", "bx-menu-alt-right");
        } else {
          btn_menu.classList.replace("bx-menu-alt-right", "bx-menu");
        }
      }

      const btn_theme = document.querySelector(".theme-btn");
      const theme_ball = document.querySelector(".theme-ball");

      const localData = localStorage.getItem("theme");

      if (localData == null) {
        localStorage.setItem("theme", "light");
      }

      if (localData == "dark") {
        document.body.classList.add("dark-mode");
        theme_ball.classList.add("dark");
      } else if (localData == "light") {
        document.body.classList.remove("dark-mode");
        theme_ball.classList.remove("dark");
      }

      btn_theme.addEventListener("click", function () {
        document.body.classList.toggle("dark-mode");
        theme_ball.classList.toggle("dark");
        if (document.body.classList.contains("dark-mode")) {
          localStorage.setItem("theme", "dark");
        } else {
          localStorage.setItem("theme", "light");
        }
      });
      function hideAllSections() {
    // Select all sections you want to hide
    const sections = [
        "introduction-content",
        "data-description-content",
        "result-content",
    
        "conclusion-content"  // Include the price direction plot section if needed
    ];

    // Hide all sections
    sections.forEach(function(id) {
        const content = document.getElementById(id);
        if (content) {
            content.classList.remove("show-content");
        }
    });
}



function showIntroduction() {
    // Hide all other sections first
    hideAllSections();

    // Show the Introduction content
    const introContent = document.getElementById("introduction-content");
    if (introContent) {
        introContent.classList.add("show-content");
    }
}

function showDataDescription() {
    // Hide all other sections first
    hideAllSections();

    // Show the Data Description content
    const dataDescriptionContent = document.getElementById("data-description-content");
    if (dataDescriptionContent) {
        dataDescriptionContent.classList.add("show-content");
    }
}

function showResults() {
    // Hide all other sections first
    hideAllSections();

    // Show the Results content
    const resContent = document.getElementById("result-content");
    if (resContent) {
        resContent.classList.add("show-content");
    }
}
function showConclusion(){
    hideAllSections();
    //show conclusion
    const conContent=document.getElementById("conclusion-content");
    if(conContent){
        conContent.classList.add("show-content");
    }

}
