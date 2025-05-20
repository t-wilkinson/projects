<script lang="ts">
	import Link from '$lib/components/Link.svelte';
</script>

<section id="welcome" class="hero">
	<img class="img" src="/IMG-4279.jpg" alt="The studio" />
	<div class="content">
		<h1>Welcome to Our Website</h1>
        <!-- <Link class="link" href="#service">Let's Create</Link> -->
	</div>
</section>

<style lang="scss">
	.hero {
		position: relative;
		display: grid;
		/* grid-template: */
		/* 	'. . .' 1fr */
		/* 	'. content .' auto */
		/* 	'. . .' 2fr */
		/* 	/ 3fr 1400px 1fr; */
        grid-template: '.' 1fr / 1fr;
		width: 100%;
		height: 100vh;
		color: white;
		justify-items: center;
		align-items: center;
        overflow: hidden;

        h1 {
            font-size: 5rem;
            text-align: center;
        }

		.img {
			position: absolute;
			top: 0;
			left: 0;
			width: 100%;
			height: 100vh;
			object-fit: cover;
            filter: blur(2px) brightness(75%);
		}

		.content {
			font-size: 0.8vw;
            grid-area: 1 / 1 / 1 / 1;
			position: relative;
			display: flex;
			flex-direction: column;
			align-items: start;
            padding: 2rem;

			h1 {
				margin-bottom: 0.25em;
			}

		}

        @media (max-width: 800px) {
            h1 {
                font-size: 3rem;
            }

        }

        :global(.link) {
            font-size: 2vw;
            @media (max-width: 800px) {
                font-size: 1rem;
            }
        }
	}
</style>
