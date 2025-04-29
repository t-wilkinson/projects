<script>
    export let contacts
</script>

<style>
    table {
        width: 100%;
    }

    th {
        text-align: left;
        font-weight: bold;
        font-size: 0.7rem;
    }

</style>

<table>
    <tr>
        <th>Role</th>
        <th>Name</th>
        <th>Phone Number</th>
    </tr>
    {#each contacts as contact}
        <tr>
            <td>{contact.role}</td>
            <td>{contact.name}</td>
            <td class="phone">
                {#if contact.phone}
                    <a class="blue-link" href="phone:{contact.phone}">{contact.phone}</a>
                {/if}
            </td>
        </tr>

        <!--
            {#if contact.email}
                <a href="email:{contact.email}">{contact.email}</a>
            {/if}
        -->
    {/each}
</table>

{#if contacts.length === 0}
    <div class="list__message--empty">
        No contacts
    </div>
{/if}

