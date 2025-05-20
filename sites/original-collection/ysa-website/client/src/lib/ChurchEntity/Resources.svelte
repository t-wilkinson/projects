<script>
    export let resources
</script>

{#each resources as resource}
    <li>
        <a class="blue-link" target="_blank" href={resource.link}>{resource.text}</a>
    </li>
{:else}
    <div class="list__message--empty">
        No resources
    </div>
{/each}

