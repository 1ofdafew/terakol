
import router from '../../main'

const URL = 'http://localhost:5000/api/auth'
export default {

  user: {
    authenticated: false,
    token: ''
  },

  options () {
    return [{
      headers: [{
        'Content-Type': 'application/json'
      }]
    }]
  },

  login (context, creds, redirect) {
    // do HTTP post to our  URL
    var data = {
      id: creds.email,
      password: creds.password
    }
    console.log('Credentials: ' + JSON.stringify(data))
    console.log('Posting to URL: ' + URL)
    context.$http.post(URL, data, this.options())
      .then(function (resp) {
        var reply = resp.data
        console.log('Reply: ' + JSON.stringify(reply))
        // save the token in our user store
        window.localStorage.setItem('auth', JSON.stringify(reply))
        context.auth = true
        router.go(redirect)
      }, function (error) {
        console.log('Error: ' + JSON.stringify(error))
        context.auth = false
      }).catch((exception) => {
        console.error('Unable to authenticate: ' + JSON.stringify(exception))
      })
  }
}
