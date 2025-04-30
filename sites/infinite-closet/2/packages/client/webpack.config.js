const path = require('path')
const { merge } = require('webpack-merge')
const { BundleAnalyzerPlugin } = require('webpack-bundle-analyzer')

// This will automatically get the dev/prod config based on process.env.NODE_ENV.
const createExpoConfig = require('@expo/webpack-config')

// Expo expects a function so we can pass around options.
module.exports = async function (env, argv) {
  const expoConfig = await createExpoConfig(env, argv)
  // Optionally you can enable the bundle size report.
  // It's best to do this only with production builds because it will add noticeably more time to your builds and reloads.

  // env.mode = 'development'
  // if (env.mode === 'development') {
  //   expoConfig.plugins.push(
  //     new BundleAnalyzerPlugin({
  //       path: 'web-report',
  //     }),
  //   )
  // }

  return merge(expoConfig, {
    resolve: {
      extensions: ['.js', '.jsx', '.ts', '.tsx', '.native.ts', '.native.tsx'],
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
        ].reduce((acc, value) =>
          Object.assign(acc, { [value]: `./src/${value}` }),
        ),
      },
    },
    optimization: {
      minimize: false,
    },
  })
}
