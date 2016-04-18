<template>
  <nav class="black lighten-1" role="navigation">
    <div class="nav-wrapper container">
      <a id="logo-container" href="#" class="brand-logo">
        <img class="logo" src="../assets/logo.png" />
      </a>
      <ul class="right hide-on-med-and-down">
        <li><a v-link="{ path: '/', activeClass: 'active' }">Home</a></li>
        <li><a v-link="{ path: '/admin', activeClass: 'active' }">Admin</a></li>
        <li><a v-link="{ path: '/signin', activeClass: 'active' }">Sign In</a></li>
        <li><button class="waves-effect waves-light btn" v-link="{ path: '/signup', activeClass: 'active' }">Sign Up</button></li>
      </ul>

      <ul id="nav-mobile" class="side-nav">
        <li><a v-link="{path: '/', activeClass: 'active'}">Home</a></li>
        <li><a v-link="{path: '/admin', activeClass: 'active'}">Admin</a></li>
        <li><a v-link="{ path: '/signin', activeClass: 'active' }">Sign In</a></li>
        <li><button class="waves-effect waves-light btn" v-link="{ path: '/signup', activeClass: 'active' }">Sign Up</button></li>
      </ul>
      <a href="#" data-activates="nav-mobile" class="button-collapse"><i class="material-icons">menu</i></a>
    </div>
  </nav>
</template>

<style>
.logo {
  width: 40px;
  height: 40px;
  margin-top: 10px;
}

</style>
