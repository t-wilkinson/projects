<script lang="ts">
	import Link from '$lib/components/Link.svelte';
	const services = [
		{
			name: 'Beats',
			text: 'We offer a wide range of genres in our beat catalog. You can listen to our selection by clicking the Soundcloud link at the bottom of the page.'
		},
		{
			name: 'Recording',
			text: 'Klean Studios has no official hours of operation. We work to the artist’s convenience and base scheduling around clientele availability. We ask you please be courteous when scheduling though.'
		},
		{
			name: 'Mixing',
			text: 'We’ll mix everything from individual vocal or instrument stems to two stereo tracks for pre-master final mixes. '
		},
		{
			name: 'Mastering',
			text: 'Send us two or three reference mixes for your track. Our mastering deal also includes one revision with no additional cost. '
		}
	];
</script>

<section class="services">
	<h2>Services</h2>
	<div class="container">
		{#each services as service}
			<div class="service">
				<h3 class="label">{service.name}</h3>
                <!-- <div class="sep" /> -->
				<p>{service.text}</p>
			</div>
		{/each}
	</div>
    <img class="image"
         alt="Room with piano, speakers, etc."
         src="/IMG-4291.jpg"
         />
     <div class="link">
         <Link href="#service">Let's Create</Link>
     </div>
</section>


<style lang="scss">
	.services {
        display: grid;
        grid-template:
            "text image" auto
            "services image" auto
            "button button" auto
            / 1fr 1fr;
        justify-items: center;
        width: 100%;
        max-width: var(--screen-lg);
		margin: 10rem 0;
		position: relative;
		padding: 0 2rem;
        gap: 1rem;

		h2 {
            font-size: 4rem;
            grid-area: text;
            justify-self: start;
            margin-bottom: 4rem;
		}

        .link {
            margin-top: 4rem;
            font-size: 2rem;
            grid-area: button;
        }

        @media (max-width: 800px) {
            padding: 0 2rem;
            margin: 4rem 0 8rem;
            grid-template:
                "image" 300px
                "text" auto
                "services" auto
                "button" auto
                / 1fr;

            h2 {
                margin-top: 1rem;
                margin-bottom: 1rem;
            }
        }

        .image {
            grid-area: image;
            position: absolute;
            width: 100%;
            height: 100%;
            object-fit: cover;
        }

		.container {
            grid-area: services;
			display: flex;
			width: 100%;
			max-width: var(--screen-lg);
			flex-wrap: wrap;
			gap: 1rem;

			.service {
				/* background: rgba(0 0 0 / 0.2); */
				padding: 1rem;
                width: 100%;
                background: var(--c-gray);

                @media (max-width: 900px) {
                    width: auto;
                }

                .label {
                    color: #efd9fb;
                    margin-bottom: 0.5rem;
                }

                p {
                    margin: 0;
                }

				.sep {
                    display: none;
                    width: 100%;
                    margin-bottom: 0.5rem;
					height: 0.25rem;
					/* background-color: var(--c-ter); */
					background-color: #8E1BCD;
				}
			}
		}
	}
</style>
