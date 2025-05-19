<script>
	import Button from '$lib/components/Button.svelte';

	let firstName = '';
	let lastName = '';
	let email = '';
	let phoneNumber = '';
	let groupName = '';

	let isGroup = false;

	let recording = false;
	let mixing = false;
	let mastering = false;

	let details = '';

	let state = 'editing';
	export let onSubmit;

	function labelToName(label) {
		return label.toLowerCase().replace(/ /g, '-');
	}
</script>

<form
	id="service-form"
	on:submit={async (e) => {
		e.preventDefault();
		state = 'submitting';
		const form = document.querySelector('#service-form');
		const formData = new FormData(form);
		onSubmit(formData)
			.then(() => {
				state = 'success';
			})
			.catch(() => {
				state = 'error';
			});
	}}
>
	{#if state === 'success'}
		<div class="success">
			<h2>Thank you</h2>
			<p>We have received your submission</p>
		</div>
	{:else if state === 'error'}
		<div class="error">
			<h2>Oops!</h2>
			<p>Looks like we're having technical difficulties!</p>
		</div>
	{:else}
		<h2>Request a Service</h2>
		<div class="inputs">
			{#each [{ label: 'First Name', type: 'text', value: firstName }, { label: 'Last Name', value: lastName, type: 'text' }, { label: 'Email', type: 'email', value: email }, { label: 'Phone Number', value: phoneNumber, type: 'text' }, { label: 'Group Name', value: groupName, type: 'text' }] as input}
				{@const name = labelToName(input.label)}
				<label for={name}>
					<span class="label">{input.label}</span>
					<input id={name} {name} bind:value={input.value} />
				</label>
			{/each}
		</div>

		<div class="checkbox">
			<label for="is-group">
				<input id="is-group" name="is-group" type="checkbox" bind:checked={isGroup} />
				<span class="label">There will be multiple attendees</span>
			</label>
		</div>

		<div class="checkbox">
			{#each [{ label: 'Recording ($25 per hour)', value: recording }, { label: 'Mixing ($35 - $125 workload based)', value: mixing }, { label: 'Mastering ($75 per track)', value: mastering }] as input}
				{@const name = labelToName(input.label)}
				<label for={name}>
					<input id={name} {name} type="checkbox" bind:checked={input.value} />
					<span class="label">{input.label}</span>
				</label>
			{/each}
		</div>

		<div class="details">
			<label for="details">
				<span class="label">Details</span>
				<textarea id="details" name="details" bind:value={details} rows={10} />
			</label>
		</div>

		<div class="submit">
			<Button type="submit">Submit</Button>
		</div>

		<ul>
			<h4 class="label">Other Info</h4>
			<li>
				<small>*</small>
				An additional $15 may be included in cost as a set up fee in band setting occurrences or chamber/orchestral
				groups.
			</li>
			<li>
				<small>*</small>
				We appreciate you coming to Klean Studios and becoming part of our growing community. So when
				bundling services you can expect discounted rates anywhere from 15% to 50% off.
			</li>
			<li>
				<small>*</small>
				After scheduling with us once, you will always have direct contact to a producer at Klean Studios
				to coordinate future sessions with.
			</li>
		</ul>
	{/if}
</form>

<style lang="scss">
	form {
		font-size: 1.2rem;
		width: 100%;

		.success,
		.error {
			text-align: center;
			h2 {
				margin: 0;
				padding: 3rem 0;
			}
		}

		h2 {
			font-size: 4rem;
			margin-bottom: 2rem;
		}

		div + div {
			margin-top: 2rem;
		}

		label {
			display: flex;
			flex-direction: column;

			.label {
				font-family: 'Abril Fatface', serif;
				margin-bottom: 0.25rem;
			}

			input {
				background: white;
				border: none;
				padding: 0.75rem 1rem;
			}
		}

		.checkbox {
			label {
				flex-direction: row;
				align-items: center;

				.label {
					margin-left: 0.5rem;
					margin-bottom: 0;
				}
			}
		}

		.inputs {
			grid-gap: 1rem;
			display: grid;
			grid-template-columns: 1fr 1fr;
		}

		textarea {
			padding: 1rem;
		}

		.submit {
			font-size: 1.5rem;
		}
	}

	ul {
		.label {
			font-size: 1.75rem;
			font-weight: bold;
			padding-bottom: 1rem;
			font-family: var(--font-secondary);
		}

		list-style: none;
		padding: 0;
		font-size: 1.1rem;
		margin-top: 4rem;

		li {
			margin: 0;
		}

		li + li {
			margin-top: 1rem;
		}
	}

	@media (max-width: 500px) {
		form {
			.inputs {
				display: flex;
				flex-direction: column;
			}
		}
	}

	@media (max-width: 600px) {
		form {
			h2 {
				font-size: 3rem;
			}
		}
	}
</style>
