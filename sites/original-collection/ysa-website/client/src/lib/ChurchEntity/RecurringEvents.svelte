<script lang="ts">
    import Event from './Event.svelte'
    export let events, meetingHouseLocation

    const dateOptions = { weekday: 'long'}
    const timeOptions = {timeStyle: 'short'}

    const daysOfWeek = {
        'sunday': 0,
        'monday': 1,
        'tuesday': 2,
        'wednesday': 3,
        'thursday': 4,
        'friday': 5,
        'saturday': 6,
    }

    const today = new Date()

    function timeToDate(time, dow) {
        // Set next day of week
        const date = new Date()
        const currentDay = date.getDay()
        const distance = (daysOfWeek[dow] + 7 - currentDay) % 7
        date.setDate(date.getDate() + distance)

        // Time of day
        const [hours, minutes, ] = time.split(':')
        date.setHours(hours, minutes, 0, 0)

        return date
    }

    function getRepetitionDays(repeats, dow) {
        return {
            weekly: `Every ${dow}`,
            firstOfMonth: `First ${dow} of every month`,
            firstAndThirdOfMonth: `First and third ${dow} of every month`,
            secondAndFourthOfMonth: `Second and fourth ${dow} of every month`,
        }[repeats]
    }

</script>

<style>
    li {
        list-style: none;
    }
</style>

{#each events as event}
    {@const start = timeToDate(event.start, event.dayOfWeek)}
    {@const dayOfWeek = start.toLocaleDateString(undefined, dateOptions)}
    {@const startTime = start.toLocaleTimeString(undefined, timeOptions)}
    {@const end = timeToDate(event.end, event.dayOfWeek)}
    {@const endTime = end.toLocaleTimeString(undefined, timeOptions)}
    {#if end >= today}
        <li>
            <Event startDate={getRepetitionDays(event.repeats, dayOfWeek)} {startTime} {endTime} name={event.name}
                   description={event.description}
                location={event.location === 'church' || !event.location ? meetingHouseLocation : event.location} />
        </li>
    {/if}
{:else}
    <div class="list__message--empty">
        No recurring events
    </div>
{/each}


