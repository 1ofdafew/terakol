<template>
  <div class="container">
    <div class="row">
      <div id="box" class="col s12 m4 offset-m4">
        <div class="card">
          <div class="card-content">
            <div class="center-align">
              <img class="logo" src="../assets/logo.png" />
            </div>
            <form id="signup" method="post">
              <h3>Sign Up</h3>
              <div class="row">
                <div class="input-field ">
                  <input id="first_name" type="text" class="validate">
                  <label for="first_name">First Name</label>
                </div>
              </div>
              <div class="row">
                <div class="input-field ">
                  <input id="last_name" type="text" class="validate">
                  <label for="last_name">Last Name</label>
                </div>
              </div>
              <div class="row">
                <div class="input-field ">
                  <input id="email" type="email" class="validate">
                  <label for="email">Email</label>
                </div>
              </div>
              <div class="row">
                <div class="input-field ">
                  <input id="password" type="password" class="validate">
                  <label for="password">Password</label>
                </div>
              </div>
              <div class="row">
                <div class="input-field ">
                  <input id="password2" type="password" class="validate">
                  <label for="password2">Confirm Password</label>
                </div>
              </div>
            </form>
          </div>
          <div class="card-action">
            <button class="waves-effect waves-light btn">SignUp</button>
            <a id="login" v-link="{path: '/signin'}">Sign In</a>
          </div>
        </div>
      </div>
    </div>
  </div>
</template>

<style scope>
#login {
  margin-left: 20px;
}
#box {
  margin-top: 40px;
}
#signup {
  padding: 20px;
}
</style>
