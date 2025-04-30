import React from 'react'
import dayjs, { Dayjs } from 'dayjs'

// Given curDay, find all days that would appear on a 7x6 calendar for curDay.Month
export const useDays = (curDay: Dayjs) => {
  const [date, setDate] = React.useState(curDay ?? dayjs())

  const prevMonth: Dayjs[] = []
  const nextMonth: Dayjs[] = []
  const curMonth: Dayjs[] = []

  const monthDate = date.date(1)

  // Previous Month
  const prevDays = monthDate.day()
  if (prevDays !== 0) {
    const prevMonthDate = monthDate.subtract(prevDays, 'day')
    for (var d = prevMonthDate.date(); d <= prevMonthDate.daysInMonth(); d++) {
      prevMonth.push(prevMonthDate.date(d))
    }
  }

  // Current Month
  for (var d = monthDate.date(); d <= monthDate.daysInMonth(); d++) {
    curMonth.push(monthDate.date(d))
  }

  // Next Month
  let nextDays = 6 - date.date(date.daysInMonth()).day()
  while (prevMonth.length + curMonth.length + nextDays < 7 * 6) {
    nextDays = nextDays + 7
  }
  const nextMonthDate = monthDate.date(date.daysInMonth()).add(1, 'day')
  for (var d = nextMonthDate.date(); d <= nextDays; d++) {
    nextMonth.push(nextMonthDate.date(d))
  }

  return {
    date,
    setDate,
    prevMonth,
    curMonth,
    nextMonth,
    days: ([] as Dayjs[]).concat(prevMonth, curMonth, nextMonth),
  }
}
export default useDays
