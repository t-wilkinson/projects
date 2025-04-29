<script lang="ts">
    import { page } from "$app/stores";
    import { getStake } from "$lib/Stake/api.ts"
    import { getImgUrl, getAttributes } from "$lib/api.ts"
    import { Layout, MapLink } from "$lib/Components"
    import { Resources, Contacts, Events } from "$lib/ChurchEntity"

    let stake = undefined
    $: {
        getStake($page.params.stake)
        .then(x => stake = x)
        .catch(err => console.error(err))
    }
</script>

<style>
    h3 {
        font: var(--typography-h3);
        margin: 2rem 0 1rem;
    }

    section:first-child h3 {
        margin-top: 0;
    }

    .wards {
        display: flex;
        width: 100%;
        overflow-x: auto;
    }

    .ward {
        overflow: hidden;
        width: 16rem;
        flex: 0 0 auto;
        background: var(--grey2);
        border-radius: 0.5rem;
        margin: 0 1rem 1rem 0;
    }

    .ward__text {
        padding: 1rem;
    }

    .ward__name {
        font-weight: 700;
        margin-bottom: 0.5rem;
    }

    .ward__photo {
        width: 100%;
        height: 16rem;
        object-fit: cover;
    }

    section {
        padding: 0 1rem;
    }
</style>

{#if stake}
    <Layout {stake}>
        <section id="wards">
        <h3>Wards</h3>
        <ul class="wards">
            {#each getAttributes(stake.wards) as ward}
                <li class="ward">
                    <a href="/{stake.slug}/{ward.slug}">
                        {#if ward.wardPhoto.data}
                            <img class="ward__photo" alt="Members in {ward.name}" src={getImgUrl(ward.wardPhoto)} />
                        {:else}
                            <div class="ward__photo" />
                        {/if}
                    </a>
                    <div class="ward__text">
                        <div>
                            <a class="ward__name" href="/{stake.slug}/{ward.slug}">
                                {ward.name}
                            </a>
                        </div>
                        <div>
                            <MapLink class="ward__location" location={ward.meetingHouseLocation} />
                        </div>
                    </div>
                </li>
            {/each}
        </ul>
        </section>

        <!-- Upcoming Events -->
        <section id="events">
            <h3>Upcoming Events</h3>
            <ul>
                <Events events={stake.events} meetingHouseLocation={stake.stakeCenterLocation} />
            </ul>
        </section>

        <!-- Resources -->
        <section id="resources">
            <h3>Resources</h3>
            <ul>
                <Resources resources={stake.resources} />
            </ul>
        </section>

        <!-- Contact -->
        <section id="contacts">
            <h3>Contacts</h3>
            <Contacts contacts={stake.contacts} />
        </section>

    </Layout>
{:else}
    <Layout />
{/if}
