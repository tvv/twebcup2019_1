'use strict'
const
  path = require('path');


var siteConfig = {
  mode: 'none',
  output: {
    filename: "index.min.js",
    path: path.resolve(__dirname, "..", '/priv/public/js')
  },
  externals: {
    sly: 'Sly',
    moment: 'moment'
  },
  module: {
    rules: [
      {
        test: /\.js$/,
        exclude: /(node_modules|bower_components)/,
        loader: 'babel-loader'
      },
      {
        test: /(\.jsx|\.js)$/,
        loader: "eslint-loader",
        exclude: /node_modules/
      },
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        use: {
          loader: 'elm-webpack-loader',
          options: {
            files: [
                './elm/Main.elm'
              ],
              verbose: false,
              warn: false,
              debug: false//,
              //optimize: true
          }
        }
      }
    ]
  },
  plugins: [
  ],
  optimization: {
    minimize: true,
    concatenateModules: true
  },
  resolve: {
    modules: [path.resolve('./node_modules'), path.resolve('./lib')],
    extensions: ['.json', '.js']
  }
};


module.exports = siteConfig;