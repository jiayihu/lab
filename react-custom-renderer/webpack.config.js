const path = require('path');
const webpack = require('webpack');

const root = {
  src: path.join(__dirname, 'index-dom.js'),
  dest: path.join(__dirname, 'dist'),
};

module.exports = {
  devServer: {
    historyApiFallback: true,
    noInfo: false,
    port: 3000,
  },
  devtool: 'eval',
  entry: {
    main: root.src,
  },
  output: {
    path: root.dest,
    filename: 'main.js',
  },
  resolve: {
    extensions: ['.js', '.jsx'],
  },
  module: {
    rules: [
      {
        test: /\.jsx?$/,
        use: [
          {
            loader: 'babel-loader',
            options: {
              cacheDirectory: true,
            },
          },
        ],
        include: root.src,
      },
    ],
  },
  plugins: [],
};
