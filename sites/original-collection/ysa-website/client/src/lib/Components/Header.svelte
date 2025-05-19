<script lang="ts">
    import Arrow from "$lib/Icons/Arrow.svelte"
    import UnofficialLDSLogo from "$lib/Icons/UnofficialLDSLogo.svelte"
    export let stake = undefined;
    export let ward = undefined;
</script>

<style>
    nav {
        box-shadow:  0 2px 4px 0 rgba(0, 0, 0, 0.15), 0 -2px 4px 0 rgba(0, 0, 0, 0.15);
        z-index: 20;
    }

    header * {
        font-family: var(--sans);
        font-size: 13px;
    }

    strong {
        font-weight: bold;
    }

</style>

<header class="flex flex-col w-full items-center">
    <a href="/">
        <div style="width: 170px" class="unofficial-lds-logo">
            <UnofficialLDSLogo />
        </div>
    </a>
    <nav class="w-full flex justify-start relative">
        <svg class="top-0 left-0" width="34" height="40" style="opacity: 0.75;" id="lightRays" xmlns="http://www.w3.org/2000/svg"><defs><linearGradient x1="0%" y1="50%" y2="50%" id="a"><stop stop-color="var(--theme-page-header-nav-bg, #01b6d1)" stop-opacity=".6" offset="0%"></stop><stop stop-color="var(--theme-page-header-nav-bg, #01b6d1)" stop-opacity=".25" offset="100%"></stop></linearGradient><linearGradient x1="0%" y1="50%" y2="50%" id="b"><stop stop-color="var(--theme-page-header-nav-bg, #01b6d1)" stop-opacity=".35" offset="0%"></stop><stop stop-color="var(--theme-page-header-nav-bg, #01b6d1)" stop-opacity=".15" offset="100%"></stop></linearGradient><linearGradient x1="0%" y1="50%" x2="98.607%" y2="50%" id="c"><stop stop-color="var(--theme-page-header-nav-bg, #01b6d1)" stop-opacity=".6" offset="0%"></stop><stop stop-color="var(--theme-page-header-nav-bg, #01b6d1)" stop-opacity=".35" offset="100%"></stop></linearGradient></defs><g fill-rule="nonzero" fill="none"><path fill="url(#a)" d="M8 40V0h10.34l-7.755 40z"></path><path fill="url(#b)" d="M8 40V3.93L33.984 40z"></path><path fill="url(#a)" d="M8 40V0l15.64 40z"></path><path fill="url(#c)" d="M10.598 40H7V0h3.598z"></path><path fill="var(--theme-page-header-nav-bg, #01b6d1)" d="M0 0h8v40H0z"></path></g></svg>
        <div class="flex items-center">
            {#if stake && ward}
                <a href="/">Stakes</a> <Arrow /> <a href="/{stake.slug}">{stake.name}</a> <Arrow /> <strong>{ward.name}</strong>
            {:else if stake}
                <a href="/">Stakes</a> <Arrow /> <strong>{stake.name}</strong>
            {:else}
                <a href="/">
                    <strong>Stakes</strong>
                </a>
                <slot />
            {/if}
        </div>
    </nav>
</header>
