<script lang="ts">
    import {MapLink} from '$lib/Components'
    import {RecurringEvents } from '$lib/ChurchEntity'

    export let ward;
    export let stake;
</script>

<style>
    .footer-wrapper {
        display: grid;
        place-items: center;
        width: 100%;
        padding: 2rem 1rem 1rem;
        margin-top: 2rem;
        background: var(--grey-2);
    }

    footer {
        display: grid;
        width: 100%;
        max-width: var(--screen-lg);
        justify-items: center;
        gap: 2rem;
        grid-template:
            "locations locations" auto
            "events events" auto
            "disclaimer disclaimer" auto
            / 1fr 1fr;

    }

    .locations { grid-area: locations; }

    .events { grid-area: events; place-self: start; width: 100%; }

    .disclaimer {
        text-align: center;
        grid-area: disclaimer;
        margin-top: 1rem;
    }

    .disclaimer strong {
        font-weight: 700;
    }

    nav {
        text-align: left;
        width: 100%;
        max-width: var(--screen-lg);
    }

    nav strong {
        font-weight: 600;
    }

    h6 {
        margin: 1rem 0;
        font-size: 1.25rem;
        font-weight: 700;
    }
</style>

<div class="footer-wrapper">
    <footer>
        {#if ward || stake}
            <nav class="locations">
                <ul>
                    {#if ward}
                        <li>
                            <strong>Meeting House Location:</strong> <MapLink location={ward.meetingHouseLocation} />
                        </li>
                    {/if}
                    {#if stake}
                        <li>
                            <strong>Stake Center Location:</strong> <MapLink location={stake.stakeCenterLocation} />
                        </li>
                    {/if}
                </ul>
            </nav>
        {/if}

        <!--
            <nav class="links">
            <ul>
            <li>
            <a href="/about">
            About this website
            </a>
            </li>
            </ul>
            </nav>
        -->

        {#if ward}
            <div class="events">
                <h6>Recurring Events</h6>
                <RecurringEvents events={ward.recurringEvents} meetingHouseLocation={ward.meetingHouseLocation} />
            </div>
        {/if}
        <small class="disclaimer"><span>This website is <strong>not</strong> an official product of "The Church of Jesus Christ of Latter-Day Saints".</span></small>
    </footer>
</div>
