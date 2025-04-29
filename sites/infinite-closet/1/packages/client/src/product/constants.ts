import { gql } from '@apollo/client'

export const items = {
  dropdown: [
    { key: 'stylistnotes', label: 'Stylist Notes' },
    { key: 'description', label: 'Product Description' },
    { key: 'share', label: 'Share' },
  ],
}

export const QUERY = gql`
  query($name_uid: String!) {
    setting {
      membership_price
    }
    shopItems(limit: 1, where: { name_uid: $name_uid }) {
      id
      name
      name_uid
      designer {
        name
        name_uid
      }
      description
      stylistnotes
      retail_price
      rental_price
      purchase_price
      images {
        url
      }
      sizes {
        id
        label
        quantity
      }
    }
  }
`
