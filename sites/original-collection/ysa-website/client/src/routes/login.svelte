<script>
    let name = '';
    let email = '';
    let password = '';
</script>

<style>
    form {
        display: flex;
        flex-direction: column;
        justify-content: center;
        align-items: center;
        width: 100%;
        height: 100%;
    }

    label + label {
        margin-top: 10px;
    }

    label {
        display: flex;
        flex-direction: column;
    }

    input {
        border: solid black 1px;
    }

    button[type="submit"] {
        border: solid black 1px;
        color: black;
        padding: 10px;
        margin-top: 10px;
    }
</style>

<form>
    <label>
        Name
        <input bind:value={name} />
    </label>

    <label>
        Email
        <input bind:value={email} />
    </label>

    <label>
        Password
        <input bind:value={password} type="password" />
    </label>

    <button type="submit">Submit</button>
</form>
