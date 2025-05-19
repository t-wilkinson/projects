<script>
</script>

<style>
    a {
        border: none;
        background: var(--c-pri);
        color: white;
        padding: 0.5em 1em;
        cursor: pointer;
        font-family: 'Abril Fatface', serif;
        text-decoration: none;
        font-size: inherit;
    }
</style>

<a {...$$restProps} class="link">
    <slot />
</a>

