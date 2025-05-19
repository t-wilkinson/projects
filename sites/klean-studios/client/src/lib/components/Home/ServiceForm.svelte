<script lang="ts">
	import ServiceSubmit from '$lib/components/Form/Service.svelte';
	import { toApiUrl } from '$lib/api.js';
</script>

<section id="service" class="service-form">
	<ServiceSubmit
		onSubmit={async (formData) => {
			const response = await fetch(toApiUrl('/service/submit'), {
				method: 'POST',
				body: formData
			});
            if (!response.ok) {
             throw response
             }
		}}
	/>
</section>

<style lang="scss">
    .service-form {
        margin: 8rem 2rem;
        max-width: var(--screen-md);
        display: flex;
        flex-direction: column;
        align-items: center;
    }
</style>
