const path = require('path')

module.exports = function (api) {
  api.cache.never()
  return {
    presets: ['babel-preset-expo'],
    plugins: [
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
            react: path.resolve('./node_modules/react'),
            assets: path.resolve(__dirname, 'assets'),
            ...[
              'products',
              'coming-soon',
              'home',
              'landing-page',
              'product',
              'privacy-policy',
              'shared',
              'user',
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
