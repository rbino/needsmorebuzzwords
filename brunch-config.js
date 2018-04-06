module.exports = {
  files: {
    javascripts: {joinTo: 'js/app.js'},
    stylesheets: {joinTo: 'css/app.css'}
  },

  plugins: {
    elmBrunch: {
      mainModules: ['app/elm/Main.elm'],
      outputFolder: 'public/js',
      makeParameters: '--debug'
    }
  },
  overrides: {
    production: {
      plugins: {
        elmBrunch: {
          makeParameters: []
        }
      }
    }
  }
}
