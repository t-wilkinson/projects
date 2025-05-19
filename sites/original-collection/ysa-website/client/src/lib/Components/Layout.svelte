<script lang="ts">
    import { Header, Footer } from "./"

    export let stake = undefined;
    export let ward = undefined;
</script>

<style>
    .main-layout {
        display: flex;
        flex-direction: column;
        min-height: 100vh;
        align-items: center;
        width: 100%;
    }

    main {
        flex: 1 0 auto;
        width: 100%;
        max-width: var(--screen-lg);
        display: flex;
        flex-direction: column;
        margin: 4rem 0;
    }
</style>

<div class="main-layout">
    <Header {stake} {ward} />
    <main>
        <slot />
    </main>
    <Footer {stake} {ward} />
</div>
