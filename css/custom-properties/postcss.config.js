const cssImport = require('postcss-import');
const cssVariables = require('postcss-css-variables');

module.exports = {
  plugins: [
    cssImport(),
    cssVariables({
      preserve: 'computed',
    }),
  ],
};
