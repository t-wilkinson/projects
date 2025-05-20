<script lang="ts">
    import { page } from '$app/stores';
    import { getStakes } from "$lib/Stake/api.ts"
    import { getAttributes } from "$lib/api.ts"
    import { Layout, Divider } from "$lib/Components"

    let stakes = []
    $: {
        getStakes()
        .then(x => stakes = x)
        .catch(err => console.error(err))
    }
</script>

<style>
    .stakes {
        font-size: 1.25rem;
    }

    .stake:first-child {
        margin-top: 0;
    }

    .stake {
        padding: 1rem;
        margin: 1rem 0;
    }

    .stake__name {
        font-size: 1.25em;
        margin-bottom: 1rem;
        font-weight: 700;
    }

    .stake__ward {
        display: inline-block;
        margin: 0.25rem 0;
        padding: 0.25rem 0;
    }
</style>

<Layout>
    <ul class="stakes">
        {#each stakes as stake}
            <li class="stake">
                <h2 class="stake__name"><a href="/{stake.slug}">{stake.name}</a></h2>
                <ul class="stake__wards">
                    {#each getAttributes(stake.wards) as ward}
                        <li>
                            <a class="stake__ward" href="/{stake.slug}/{ward.slug}">{ward.name}</a>
                        </li>
                    {/each}
                </ul>
            </li>
        {/each}
    </ul>
</Layout>
