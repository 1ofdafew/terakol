<template>
  <div class="container">
    <div class="row">
      <div id="box" class="col s12 m6 offset-m3">
        <div class="card">
          <div class="card-content">
            <div class="center-align">
              <img class="logo" src="../assets/logo.png" />
            </div>
            <form id="signup">
              <h3>Sign In</h3>
              <div class="row">
                <div class="input-field ">
                  <i class="material-icons prefix">account_circle</i>
                  <input id="email" type="email" class="validate active" v-model="creds.email">
                  <label for="email" data-error="Invalid email..">Email</label>
                </div>
              </div>
              <div class="row">
                <div class="input-field ">
                  <i class="material-icons prefix">lock</i>
                  <input id="password" type="password" class="validate active" v-model="creds.password">
                  <label for="password">Password</label>
                </div>
              </div>
            </form>
          </div>
          <div class="card-action">
            <button class="waves-effect waves-light btn" @click="signIn">Sign In</button>
            <a id="login" v-link="{path: '/signup'}">Sign Up</a>
          </div>
        </div>
      </div>
    </div>
  </div>
</template>

<script>
import auth from '../lib/auth'

export default {
  ready () {
  },
  data () {
    return {
      auth: false,
      creds: {
        email: '',
        password: ''
      }
    }
  },
  methods: {
    signIn () {
      auth.login(this, this.creds, '/')
      if (this.auth === false) {
        console.log('Invalid login...')
      }
    }
  }
}
</script>

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
