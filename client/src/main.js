import Vue from 'vue'

import App from './App'
import Home from './components/Home'
import Admin from './components/Admin'
import SignUp from './components/SignUp'
import SignIn from './components/SignIn'

var VueRouter = require('vue-router')
Vue.use(require('vue-resource'))
Vue.use(VueRouter)

var router = new VueRouter()
router.map({
  '/': { component: Home },
  '/admin': { component: Admin },
  '/signup': { component: SignUp },
  '/signin': { component: SignIn }
})
router.start(App, '#app')

/* eslint-disable no-new */
// new Vue({
//   el: 'body',
//   components: { App }
// })
