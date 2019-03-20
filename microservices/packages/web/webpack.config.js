const path = require('path');
const webpack = require('webpack');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const CopyWebpackPlugin = require('copy-webpack-plugin');

const IS_DEV = process.env.NODE_ENV !== 'production';

const htmlConfig = {
  template: './index.template.html',
  filename: './index.html',
  favicon: 'favicon.ico',
};
const envConfig = {
  'process.env': {
    NODE_ENV: JSON.stringify(process.env.NODE_ENV),
  },
};

const devPlugins = [new HtmlWebpackPlugin(htmlConfig), new webpack.DefinePlugin(envConfig)];
const prodPlugins = [
  new HtmlWebpackPlugin(htmlConfig),
  new CopyWebpackPlugin([
    { from: './favicon.ico', to: './favicon.ico' },
    { from: './assets', to: './assets' },
  ]),
  new webpack.DefinePlugin(envConfig),
];

module.exports = {
  mode: IS_DEV ? 'development' : 'production',
  devServer: {
    historyApiFallback: true,
  },
  devtool: 'eval',
  entry: './src/index.ts',
  output: {
    path: path.resolve(__dirname, IS_DEV ? '' : 'dist'),
    filename: 'index.js',
  },
  resolve: {
    extensions: ['.ts', '.ts', '.js'],
  },
  performance: { hints: false },
  module: {
    rules: [
      {
        test: /\.css$/,
        use: ['raw-loader', 'postcss-loader'],
      },
      { test: /\.tsx?$/, loader: 'ts-loader' },
      {
        test: /\.html$/,
        use: 'raw-loader',
        include: [path.resolve(__dirname, 'src')],
      },
    ],
  },
  plugins: IS_DEV ? devPlugins : prodPlugins,
};
