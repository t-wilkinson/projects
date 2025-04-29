import React from 'react'

import { Dayjs } from 'dayjs'
import DropDownPicker from 'react-native-dropdown-picker'
import { Ionicons, MaterialIcons } from '@expo/vector-icons'

import { Box, Text, TouchableOpacity, CallToAction } from 'shared/components'

import { OneTime } from './types'
import DatePicker from './DatePicker'

export const ProductRentContents = ({ shopItem, state, setState }) => {
  const [visible, setVisible] = React.useState(false)
  const [selectedDate, setSelectedDate] = React.useState<Dayjs>()

  return (
    <Box height={250}>
      {productRentContents[state.rentType]({
        // TODO performance?
        shopItem,
        setState,
        visible,
        setVisible,
        selectedDate,
        setSelectedDate,
        state,
      })}
    </Box>
  )
}
export default ProductRentContents

const rentalLengths: { [key in OneTime]: number } = {
  Short: 4,
  Long: 8,
}

const productRentContents = {
  OneTime: ({
    setState,
    shopItem,
    visible,
    setVisible,
    state,
    selectedDate,
    setSelectedDate,
  }) => (
    <>
      <DatePicker
        visible={visible}
        setVisible={setVisible}
        rentalLength={rentalLengths[state.oneTime]}
        selectedDate={selectedDate}
        setSelectedDate={setSelectedDate}
      />

      <SelectorItem label="Size" zIndex={10} width={100}>
        <DropDownPicker
          containerStyle={{ zIndex: 10 }}
          style={{ zIndex: 10 }}
          items={shopItem.sizes.map((v: { id: string } & unknown) => ({
            ...v,
            value: v.id,
          }))}
          itemStyle={{ justifyContent: 'flex-start' }}
          placeholder="Select"
          onChangeItem={(item) =>
            setState((s: { id: string } & unknown) => ({
              ...s,
              size: item.id,
            }))
          }
        />
      </SelectorItem>
      <SelectorItem label="Rental time">
        <Box flexDirection="row" justifyContent="space-between">
          <Box mr="lg">
            <OneTimeRadioButton
              setState={setState}
              selected={state.oneTime === 'Short'}
              oneTime={'Short'}
            />
            <OneTimeRadioButton
              setState={setState}
              selected={state.oneTime === 'Long'}
              oneTime={'Long'}
            />
          </Box>
          <TouchableOpacity
            onPress={() => setVisible(true)}
            style={{ flex: 1 }}
          >
            <Box
              borderWidth={1}
              borderColor="light-gray"
              borderRadius={4}
              flexDirection="row"
              flex={1}
              justifyContent="space-between"
              alignItems="center"
              px="sm"
            >
              <Text>
                {selectedDate &&
                  selectedDate.format('dd M/D') +
                    ' - ' +
                    selectedDate
                      .add(rentalLengths[state.oneTime], 'day')
                      .format('dd M/D')}
              </Text>
              <MaterialIcons name="date-range" size={24} color="black" />
            </Box>
          </TouchableOpacity>
        </Box>
      </SelectorItem>
      <CallToAction onPress={() => {}} width="100%" my="sm">
        Add to Closet
      </CallToAction>
    </>
  ),

  Membership: () => (
    <Box justifyContent="center" alignItems="center" flex={1}>
      <Text variant="subheader" textAlign="center">
        Coming Soon!
      </Text>
    </Box>
  ),

  Purchase: () => (
    <Box justifyContent="center" alignItems="center" flex={1}>
      <Text variant="subheader" textAlign="center">
        Coming Soon!
      </Text>
    </Box>
  ),
}

const SelectorItem = ({ label, children, ...props }) => (
  <Box my="sm" {...props}>
    <Text variant="body-bold" my="sm">
      {label}
    </Text>
    {children}
  </Box>
)

const OneTimeRadioButton = ({ setState, selected, oneTime }) => (
  <TouchableOpacity
    style={{ flexDirection: 'row', alignItems: 'center' }}
    onPress={() =>
      setState((s: any) => ({
        ...s,
        oneTime,
      }))
    }
  >
    <TouchableOpacity>
      <Ionicons
        name={selected ? 'radio-button-on' : 'radio-button-off'}
        size={16}
      />
    </TouchableOpacity>
    <Text>{oneTime}-day rental</Text>
  </TouchableOpacity>
)
