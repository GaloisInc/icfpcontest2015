var webpack = require('webpack')
var DedupePlugin = require('webpack/lib/optimize/DedupePlugin')
var UglifyJsPlugin = require('webpack/lib/optimize/UglifyJsPlugin')

module.exports = {
  context: __dirname,
  entry: {
    app: './lib/app.js'
  },
  output: {
    path: __dirname + '/dist',
    filename: '[name].js'
  },
  plugins: [
    new webpack.DefinePlugin({
      'process.env': {
        'NODE_ENV': '"production"'
      }
    }),
    new DedupePlugin(),
    new UglifyJsPlugin({ compress: { warnings: false } }),
  ],
  module: {
    loaders: [
      { test: /\.jsx?$/, loader: 'babel-loader' }
    ]
  },
  devtool: 'source-map'
}
