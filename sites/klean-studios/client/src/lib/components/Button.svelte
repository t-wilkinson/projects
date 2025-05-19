<script>
</script>

<style>
    button {
        font-size: inherit;
        border: none;
        background: var(--c-pri);
        color: white;
        padding: 0.5em 1em;
        cursor: pointer;
        font-family: 'Abril Fatface', serif;
    }
</style>

<button on:click {...$$restProps}>
    <slot />
</button>
