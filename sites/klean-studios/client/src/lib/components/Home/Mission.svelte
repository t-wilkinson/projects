<section id="mission" class="mission">
	<div class="container">
		<img class="image" src="IMG-4290.jpg" alt="Mission" />
		<div class="text">
			<h2>Our Mission</h2>
			<p>
				Our mission is to create a comfortable workspace as well as provide industry standard
				production. We believe in collaboration and using the strengths of others to better portray
				the message you intend to deliver. When coming to the
				studio you should feel free to express yourself and speak openly about ideas. Your vision is
				our priority.
			</p>
		</div>
	</div>
</section>

<style lang="scss">
	.mission {
		/* background: var(--c-gray); */
        background: #463c50;
		width: 100%;
		display: flex;
		justify-content: center;

		.container {
			display: grid;
			max-width: var(--screen-lg);
			grid-template:
				'image text' 100%
				/ 1fr 1fr;
			place-items: center;

			.image {
				grid-area: image;
				width: 100%;
                height: 100%;
                object-fit: cover;
			}

			.text {
				grid-area: text;
				padding: 0 2rem;
			}

            @media (max-width: 600px) {
                grid-template:
                    'image' 300px
                    'text' auto
                    / 1fr;

                .text {
                    padding: 4rem 2rem;
                }
            }
		}
	}

    @media (max-width: 1020px) {
        h2 {
            font-size: 3rem;
        }
    }

    @media (max-width: 650px) {
        .mission {
            padding-bottom: 8rem;
        }
    }
</style>
