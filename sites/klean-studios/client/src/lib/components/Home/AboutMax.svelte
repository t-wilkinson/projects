<section class="about-max">
	<img class="image" src="/max-profile.jpg" alt="Max" />

	<span class="about-me">
		About<br />Me
	</span>

	<span class="text">
		<p>
            Graduating in May of 2022 from Mary Baldwin University, Maxwell Klimczak (aka Klean) received a degree in music with emphasis in composition as well as completed extracurricular training in mastering audio. Over the past five years he has worked with many artists in a multitude of genres, including rock, rap/hip hop, RnB, indie, pop, and jazz. With experience in recording/producing live bands and doing studio production he is equipped with all the skills of a full stack audio engineer and can be used as a producer or to oversee your own production.
		</p>
	</span>

	<div class="social-media">
        {#each [
            { name: 'Instagram', alt: 'Instagram@klean__official', href: 'https://www.instagram.com/klean_official/', src: '' },
            { name: 'Soundcloud', alt: 'Soundcloud@kleanstudios', href: 'https://soundcloud.com/klean_profile', src: '' },
            ] as socialMedia}
			<a target="_blank" href={socialMedia.href}>
				{socialMedia.name}
				<!-- <img alt={socialMedia.alt} src={socialMedia.src} /> -->
			</a>
		{/each}
	</div>
</section>

<style lang="scss">
	.about-max {
		display: grid;
		width: 100%;
        max-width: var(--screen-lg);
        margin: 10rem 0;
		grid-template:
			'image text' 800px
			'image social-media' auto
			/ 1fr 1fr;
        position: relative;

		.image {
			grid-area: image;
			width: 100%;
			height: 100%;
			object-fit: cover;
		}

		.about-me {
            top: 0;
            right: 0;
			align-self: start;
			justify-self: end;
			position: absolute;

			text-transform: uppercase;
			text-align: right;
			line-height: 1;
            font-size: clamp(1rem, 12vw, 8rem);
			font-family: var(--font-secondary);
            margin-right: 1rem;
		}

		.text {
			padding: 2rem;
			margin: 0 0 0rem 0rem;
            align-self: end;
			position: relative;
            grid-area: text;
			text-align: left;
            width: 100%;
		}

		.social-media {
			display: flex;
			flex-direction: column;
			justify-self: end;
			text-align: right;
            grid-area: social-media;
			padding-right: 1rem;

			a {
				color: white;
			}
		}


		@media only screen and (max-width: 700px) {
            margin-top: 0;
            margin-bottom: 4rem;
			grid-template:
				'image' 500px
				'text' auto
				'social-media' auto
				/ 1fr;

            .image {
                grid-area: image;
            }
            .about-me {
                grid-area: image;
                font-size: 16vw;
            }
            .text {
                margin: 0;
                align-self: auto;
                justify-self: auto;
                grid-area: text;
                transform: none;
                background: none;
            }
            .social-media {
                grid-area: social-media;
                width: 100%;
                padding-bottom: 1rem;
            }
        }
	}

</style>
