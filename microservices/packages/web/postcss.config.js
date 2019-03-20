const cssImport = require('postcss-import');
const cssnext = require('postcss-cssnext');

const IS_DEV = process.env.NODE_ENV !== 'production';

const devPlugins = [
  cssImport({
    path: ['src/css/'],
  }),
];

const prodPlugins = [...devPlugins];

module.exports = {
  plugins: IS_DEV ? devPlugins : prodPlugins,
};
