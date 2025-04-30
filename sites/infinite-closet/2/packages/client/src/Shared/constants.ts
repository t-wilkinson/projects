import Constants from 'expo-constants'

export const extras = Constants.manifest.extra

export const socialMediaLinks = {
  facebook: 'https://www.facebook.com/InfiniteClosetUK',
  instagram: 'https://www.instagram.com/infinitecloset.uk/',
  twitter: 'https://twitter.com/_infinitecloset',
  tiktok: 'https://www.tiktok.com/@infinitecloset',
} as const

export const linking = {
  prefixes: extras.prefixes,
  config: {
    screens: {
      Home: '',
      LandingPage: 'landing-page',
      ComingSoon: 'coming-soon',
      PrivacyPolicy: 'privacy-policy',
      Shop: 'Shop/:designer_uid/:name_uid',
      Products: 'Products/:product',
    },
  },
} as const

const routes = [
  {
    label: 'Plans',
    value: 'plans',
    to: '/coming-soon',
    img: null,
    data: [
      {
        label: 'Category',
        to: '/coming-soon',
        data: [
          { label: 'How it works', to: '/coming-soon' },
          { label: 'Membership', to: '/coming-soon' },
          { label: 'Pick a plan', to: '/coming-soon' },
          { label: 'Customer feedback', to: '/coming-soon' },
          { label: 'Ambassador program', to: '/coming-soon' },
          { label: 'University partners', to: '/coming-soon' },
        ],
      },
    ],
  },
  {
    label: 'Trending',
    value: 'trending',
    to: '/coming-soon',
    img: null,
    data: [
      {
        label: 'Category',
        to: '/coming-soon',
        data: [
          { label: 'Popular', to: '/coming-soon' },
          { label: 'New In', to: '/coming-soon' },
          { label: 'Top rated', to: '/coming-soon' },
          { label: 'Our edit picks', to: '/coming-soon' },
          { label: 'Brand spotlight', to: '/coming-soon' },
        ],
      },
    ],
  },
  {
    label: 'Clothing',
    value: 'clothing',
    to: '/shop',
    img: null,
    data: [
      {
        label: 'Category',
        to: '/coming-soon',
        data: [
          { label: 'Dresses', to: '/coming-soon' },
          { label: 'Tops', to: '/coming-soon' },
          { label: 'Outerwear', to: '/coming-soon' },
          { label: 'Pants', to: '/coming-soon' },
          { label: 'Skirts', to: '/coming-soon' },
          { label: 'Gowns', to: '/coming-soon' },
          { label: 'Jumpsuits', to: '/coming-soon' },
          { label: 'Maternity', to: '/coming-soon' },
          { label: 'Jumpers', to: '/coming-soon' },
        ],
      },
      {
        label: 'Occasions',
        to: '/coming-soon',
        data: [
          { label: 'Wedding', to: '/coming-soon' },
          { label: 'Night Out', to: '/coming-soon' },
          { label: 'Dinner', to: '/coming-soon' },
          { label: 'Date night', to: '/coming-soon' },
          { label: 'Office', to: '/coming-soon' },
          { label: 'WFH & Loungewear', to: '/coming-soon' },
          { label: 'Brunch', to: '/coming-soon' },
          { label: 'Party', to: '/coming-soon' },
          { label: 'Weekend', to: '/coming-soon' },
        ],
      },
    ],
  },
  {
    label: 'Accessories',
    value: 'accessories',
    to: '/coming-soon',
    img: null,
    data: [
      {
        label: 'Category',
        to: '/coming-soon',
        data: [
          { label: 'Bags', to: '/coming-soon' },
          { label: 'Jewelry', to: '/coming-soon' },
          { label: 'Bridal', to: '/coming-soon' },
        ],
      },
    ],
  },
  {
    label: 'Designers',
    value: 'designers',
    to: '/coming-soon',
    img: null,
    data: [
      {
        label: 'Category',
        to: '/coming-soon',
        data: [],
      },
      {
        label: 'Trending Now',
        to: '/coming-soon',
        data: [],
      },
    ],
  },
  {
    label: 'Sale',
    value: 'sale',
    to: '/coming-soon',
    img: null,
    data: [
      {
        label: 'Category',
        to: '/coming-soon',
        data: [
          { label: 'Under £50', to: '/coming-soon' },
          { label: 'Under £100', to: '/coming-soon' },
          { label: 'Under £150', to: '/coming-soon' },
          { label: 'Under £200', to: '/coming-soon' },
        ],
      },
    ],
  },
  {
    label: 'Blog',
    value: 'blog',
    to: '/coming-soon',
    img: null,
    data: [
      {
        label: 'Category',
        to: '/coming-soon',
        data: [
          { label: 'Featured', to: '/coming-soon' },
          { label: 'Popular', to: '/coming-soon' },
        ],
      },
    ],
  },
] as const

export { routes }
