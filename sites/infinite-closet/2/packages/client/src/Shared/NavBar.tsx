import React from 'react'

import { routes } from 'Shared/constants'
import { Image, Box, Text } from 'Shared/components'
import { Link } from 'Shared/Link'
import { Wrapper } from 'Shared/Wrapper'
import Hoverable from 'Shared/Hoverable'

export const NavBar = () => {
  const [state, setState] = React.useState<{ visible: string | null }>({
    visible: null,
  })

  return (
    <Hoverable
      onHoverOut={() => setState((s) => ({ ...s, visible: null }))}
      style={{ width: '100%', zIndex: 10 }}
    >
      <Box zIndex={10} width="100%" pb="md">
        <Box alignItems="center">
          <Box width="100%" alignItems="center" justifyContent="center">
            <Box
              flexDirection="row"
              justifyContent="space-between"
              maxWidth={{ tablet: 500, laptop: 650 }}
              width="100%"
            >
              {routes.map(({ value, label, to }, i) => (
                <Hoverable
                  onHoverIn={() => setState((s) => ({ ...s, visible: value }))}
                  key={i}
                >
                  <Box
                    bg={state.visible === value ? 'light-gray' : undefined}
                    pb="md"
                    style={{ marginBottom: -16 }}
                  >
                    <Link to={to}>
                      <Text p="sm" fontSize={{ tablet: 14, laptop: 18 }}>
                        {label}
                      </Text>
                    </Link>
                  </Box>
                </Hoverable>
              ))}
            </Box>
          </Box>

          <Box
            width="100%"
            position="absolute"
            bottom={0}
            // @ts-ignore
            style={{ transform: [{ translateY: '100%' }, { translateY: 16 }] }}
            bg="light-gray"
          >
            <Wrapper>
              {routes.map((route, i) => (
                <Box
                  key={i}
                  visible={route.value === state.visible}
                  flexDirection="row"
                  p="xl"
                >
                  <Box height={300} width={400}>
                    <Image
                      style={{ flex: 1 }}
                      source={
                        route.img ??
                        require('assets/brand/Logo-Lockup---Gray.jpg')
                      }
                    />
                  </Box>

                  <Box>
                    <Box flexDirection="row">
                      {route.data.map((column, i) => (
                        <Box key={i}>
                          <Text px="md" variant="body-bold">
                            <Link
                              hoverable
                              style={{ padding: 4 }}
                              to={column.to}
                            >
                              <Text variant="body-bold" fontSize={14}>
                                {column.label}
                              </Text>
                            </Link>
                          </Text>
                          {column.data.map((row, i) => (
                            <Box key={i}>
                              <Text px="md" fontSize={14}>
                                <Link
                                  hoverable
                                  style={{ padding: 4 }}
                                  to={row.to}
                                >
                                  <Text fontSize={13}>{row.label}</Text>
                                </Link>
                              </Text>
                            </Box>
                          ))}
                        </Box>
                      ))}
                    </Box>
                  </Box>

                  <Box>
                    <Text px="md" variant="body-bold" p="xs">
                      More coming soon!
                    </Text>
                  </Box>
                </Box>
              ))}
            </Wrapper>
          </Box>
        </Box>
      </Box>
    </Hoverable>
  )
}
export default NavBar
