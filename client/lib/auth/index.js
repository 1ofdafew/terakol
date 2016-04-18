
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
      .then((resp) => {
        var reply = resp.data
        console.log('Reply: ' + JSON.stringify(reply.basic))
      }).catch((error) => {
        console.error('Unable to authenticate: ' + JSON.stringify(error))
      })
  }
}
