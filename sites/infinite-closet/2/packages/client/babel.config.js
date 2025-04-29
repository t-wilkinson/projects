// @generated: @expo/next-adapter@2.1.68
// Learn more: https://github.com/expo/expo/blob/master/docs/pages/versions/unversioned/guides/using-nextjs.md#shared-steps

// module.exports = {presets: ['@expo/next-adapter/babel']};

// @generated: @expo/next-adapter@2.1.67
// Learn more: https://github.com/expo/expo/blob/master/docs/pages/versions/unversioned/guides/using-nextjs.md#shared-steps

const path = require('path')

module.exports = function (api) {
  api.cache.never()
  return {
    presets: [
      '@babel/preset-env',
      'babel-preset-expo',
      '@expo/next-adapter/babel',
      'next/babel',
      // '@babel/preset-typescript',
    ],
    plugins: [
      '@babel/plugin-proposal-class-properties',
      // 'module:next-transpile-modules',
      [
        'module-resolver',
        {
          extensions: [
            '.js',
            '.jsx',
            '.ts',
            '.tsx',
            '.native.ts',
            '.native.tsx',
          ],
          alias: {
            // react: path.resolve('./node_modules/react'),
            assets: path.resolve(__dirname, 'assets'),
            ...[
              'Products',
              'ComingSoon',
              'Home',
              'LandingPage',
              'Product',
              'PrivacyPolicy',
              'Shared',
              'User',
            ].reduce(
              (acc, value) => Object.assign(acc, { [value]: `./src/${value}` }),
              {},
            ),
          },
        },
      ],
    ],
  }
}
