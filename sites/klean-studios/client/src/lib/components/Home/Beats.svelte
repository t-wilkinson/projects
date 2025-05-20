<script>
    import { onMount } from 'svelte'
    let safariAgent = false;
    /* onMount(() => { */
    /*     const userAgent = (agent) => RegExp(agent, 'i').test(window.navigator.userAgent) */
    /*     safariAgent = userAgent('safari') */
    /* }) */
</script>

<section id="beats" class="beats">
    <div class="beats-wrapper">
         {#if safariAgent}
             <div class="no-marquee">
                 <h2>BEATS</h2>
             </div>
         {:else}
             <div class="marquee">
                 <h2>BEATS   BEATS   BEATS   BEATS   </h2>
             </div>
         {/if}
    </div>
	<a href="https://soundcloud.com/kleanstudios?utm_source=clipboard&utm_medium=text&utm_campaign=social_sharing" target="_blank">
		<img src="/soundcloud-logo-white.png" alt="Soundcloud logo" />
	</a>
</section>

<style lang="scss">
	.beats {
		height: 100vh;
		width: 100%;
		position: relative;
		display: grid;
		grid-template: 1fr / 1fr;
		align-items: start;
		justify-items: center;
        align-content: center;
		padding: 8rem 0 12rem 0;
        overflow: hidden;

		background: linear-gradient(to bottom, #f89810 0%, #f8320f 100%);

		img {
			width: 100%;
			max-width: 700px;
			height: 700px;
			object-fit: contain;
			position: relative;
		}

        .beats-wrapper {
            display: flex;
            align-items: center;
            flex-direction: column;

            h2 {
                white-space: pre;
                display: inline-block;
                color: var(--c-bg);
            }

            .marquee {
                opacity: 0.5;
                top: 50%;
                position: absolute;
                left: 0;
                animation: -scroll-text 30s linear infinite;
                overflow: hidden;

                h2 {
                    font-size: min(70vw, 50rem);
                }
            }

            .no-marquee {
                position: absolute;
                bottom:0;
                h2 {
                    font-size: 20vw;
                }
            }
        }

        @keyframes -scroll-text {
            0% { transform: translate(0, -56%); }
            100% { transform: translate(-50%, -56%); }
        }

        @media (max-width: 800px) {
            @keyframes -scroll-text {
                0% { transform: translate(0, -56%); }
                100% { transform: translate(-50%, -56%); }
            }

            .beats-wrapper .marquee {
                h2 {
                    font-size: 15vh;
                }
            }
        }

	}

</style>
