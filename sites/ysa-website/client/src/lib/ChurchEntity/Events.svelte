<script>
    import { Event } from '$lib/ChurchEntity'
    export let events, meetingHouseLocation

    const dateOptions = { weekday: 'long', month: 'long', day: 'numeric' }
    const timeOptions = {timeStyle: 'short'}
    const today = new Date().setHours(0, 0, 0, 0)
</script>

<style>
    .event {
        margin-bottom: 1rem;
    }
</style>

{#each events as event}
    {@const start = new Date(event.start)}
    {@const startDate = start.toLocaleDateString(undefined, dateOptions)}
    {@const startTime = start.toLocaleTimeString(undefined, timeOptions)}
    {@const end = new Date(event.end)}
    {@const endTime = end.toLocaleTimeString(undefined, timeOptions)}
    {#if end >= today}
        <li class="event">
            <Event
                {startDate} {startTime} {endTime}
                description={event.description}
                name={event.name}
                location={event.location === 'church' || !event.location ? meetingHouseLocation : event.location}
            />
        </li>
    {/if}
{:else}
    <div class="list__message--empty">
        No upcoming events
    </div>
{/each}

