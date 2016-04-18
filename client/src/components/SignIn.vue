<template>
  <div class="container">
    <div class="row">
      <div id="box" class="col s12 m4 offset-m4">
        <div class="card">
          <div class="card-content">
            <div class="center-align">
              <img class="logo" src="../assets/logo.png" />
            </div>
            <form id="signup">
              <h3>Sign In</h3>
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
            </form>
          </div>
          <div class="card-action">
            <button class="waves-effect waves-light btn">Sign In</button>
            <a id="login" v-link="{path: '/signup'}">Sign Up</a>
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
