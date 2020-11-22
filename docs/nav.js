const navIconMobile = document.querySelector(".nav-icon-mobile");
const navBar = document.querySelector(".flex-container-nav-bar");
const navBarText = document.querySelectorAll(".nav-bar a");

const navSlide = () => {
  navIconMobile.addEventListener("click", function () {
    navBar.classList.toggle("nav-bar-active");
  });
};
navSlide();
