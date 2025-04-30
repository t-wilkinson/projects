<script lang="ts">
    import { MapLink } from "$lib/Components"
    import Clock from "$lib/Icons/Clock.svelte"
    import Description from "$lib/Icons/Description.svelte"
    export let name, startDate, startTime, endTime, location, description = undefined
</script>

<style>
    .event {
        margin-bottom: 1rem;
        box-shadow: var(--boxShadowRaised);
        padding: 1rem 1rem;
        background: white;
        display: grid;
        grid-template-columns: auto 1fr;
        gap: 0.25rem 1rem;
    }

    .event:nth-child(odd) {
        place-self: center;
    }

    .event__name {
        font-weight: 500;
        font-size: 1.5rem;
        margin-bottom: 0.5rem;
    }

    .event__description {
        font: var(--typography-description);
    }
</style>

<div class="event">
    <div />
    <div class="event__name">
        {name}
        {#if description}
            <div class="event__description">
                {description}
            </div>
        {/if}
    </div>

    <Clock width="20" height="20" />
    <div class="event__time">{startDate}&nbsp;&nbsp;&sdot;&nbsp;&nbsp;{startTime} - {endTime}</div>

    {#if location}
        <svg focusable="false" width="24" height="24" viewBox="0 0 24 24"><path d="M12 2C8.13 2 5 5.13 5 9c0 5.25 7 13 7 13s7-7.75 7-13c0-3.87-3.13-7-7-7zM7 9c0-2.76 2.24-5 5-5s5 2.24 5 5c0 2.88-2.88 7.19-5 9.88C9.92 16.21 7 11.85 7 9z"></path><circle cx="12" cy="9" r="2.5"></circle></svg>
        <div class="event__location">
            <MapLink {location} />
        </div>
    {/if}

    <!--
    {#if description}
        <Description width="20" height="20" />
        <div class="event__description">
            {description}
        </div>
    {/if}
    -->
</div>
