<script lang="ts">
    import SvelteMarkdown from 'svelte-markdown'
    import { page } from '$app/stores';
    import { getImgUrl } from "$lib/api.ts"
    import { getWardData } from "$lib/Ward/api.ts"
    import { Layout } from "$lib/Components"
    import { Resources, Contacts, Events } from "$lib/ChurchEntity"

    let ward = undefined
    $: {
        getWardData($page.params.ward)
        .then(x => ward = x)
        .catch(err => console.error(err))
    }
</script>

<style>
    h1 {
        font-size: 2rem;
        margin-bottom: 1rem;
        text-align: center;
    }

    h3 {
        font: var(--typography-h3);
        margin: 4rem 0 1rem;
    }

    section:not(#hero-section) {
        padding-left: 1rem;
        padding-right: 1rem;
    }

    section:first-child h3 {
        margin-top: 0;
    }

    .ward-photo {
        position: relative;
        margin: 1rem 0 1rem;
        height: 500px;
        width: 100%;
        overflow: hidden;
    }

    .ward-photo--primary {
        position: absolute;
        width: 100%;
        height: 100%;
        object-fit: contain;
        object-position: center center;
    }

    .ward-photo--blur {
        position: absolute;
        width: 100%;
        height: 100%;
        object-fit: cover;
        object-position: center center;
        filter: blur(5px);
        transform: scale(1.1);
        z-index: -10;
    }

    #hero-section {
        position: relative;
        display: grid;
        place-items: center;
    }

    #about-us {
        background: var(--grey3);
        width: 100%;
        display: flex;
        flex-direction: column;
        align-items: center;
        margin-top: 4rem;
        padding-bottom: 4rem;
    }

    footer {
        background: var(--footerGrey);
        padding: 1rem;
    }
</style>

{#if ward}
<Layout {ward} stake={ward.stake?.data.attributes}>
    {#if ward.wardPhoto.data}
        <section id="hero-section">
            <h1>{ward.name}</h1>
            <div class="ward-photo">
                <img class="ward-photo--primary" alt="Members in {ward.name}" src={getImgUrl(ward.wardPhoto)} />
                <img class="ward-photo--blur" alt="" src={getImgUrl(ward.wardPhoto)} />
            </div>
        </section>
    {/if}

    <!-- Upcoming Events -->
    <section id="events">
        <h3>Upcoming Events</h3>
        <ul>
            <Events events={ward.events} meetingHouseLocation={ward.meetingHouseLocation} />
        </ul>
    </section>

    <!-- About Us -->
    {#if ward.aboutUs}
        <section id="about-us">
            <h3>About Us</h3>
          <SvelteMarkdown source={ward.aboutUs} />
        </section>
    {/if}

    <!-- Resources -->
    <section id="resources">
        <h3>Resources</h3>
        <ul>
            <Resources resources={ward.resources} />
        </ul>
    </section>

    <!-- Newsletter -->
    <!--
    <section id="newsletter">
        <h3>Newsletter</h3>
    </section>
    -->

    <!-- Contact -->
    <section id="contacts">
        <h3>Contacts</h3>
        <Contacts contacts={ward.contacts} />
    </section>
</Layout>
{:else}
    <Layout />
{/if}

