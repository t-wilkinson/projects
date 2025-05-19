<section class="the-space">
	<img src="/IMG-4258.jpg" alt="The space" />
	<div class="content">
		<h2>The Space</h2>
		<p>
			Klean Studios is a small business that specializes in recording and producing music. We work
			with independent artists or bands as well as indie labels, major labels, and other
			producer/audio engineers. Capable of providing guidance or directly working on the production
			of a track, any service you need can be fulfilled by us. At least one engineer will always be
			present during studio sessions to provide either assistance in the creation process and/or
			overlook the production of your track.
		</p>
	</div>
</section>

<style lang="scss">
    .the-space {
        display: grid;
        position: relative;
        height: 100vh;
        width: 100%;
        place-items: center;
        padding: 0 1rem;

        .content {
            text-align: center;
            width: 100%;
            max-width: var(--screen-md);
            position: relative;
            /* background: rgba(73 79 96/ 0.65); */
            background: rgba(31 34 43/ 0.65);
            padding: 2rem;

                h2 {
                    font-size: 8rem;
                    margin-bottom: 2rem;
                }

                p {
                    font-size: 1.25rem;
                }

                @media (max-width: 900px) {
                    padding: 1rem;

                    h2 {
                        font-size: 4rem;
                        margin-bottom: 1rem;
                    }
                    p {
                        font-size: 1.1rem;
                    }
                }
        }

            img {
                position: absolute;
                object-fit: cover;
                width: 100%;
                height: 100%;
            }

    }


</style>
