<script lang="ts">
	import { fly } from 'svelte/transition';
let processes = [
        {
        img: '/IMG-4087.jpg',
        alt: '',
        label: 'Introduction and Planning',
        text: 'Before we begin working we will listen to reference tracks and discuss what we want the end result of your song to be like. This includes listing instruments, genre, bpm, fx, and other important details about your song that are necessary for the producer to know. This step is important because it establishes the direction we are going in from the very jump and helps provide a clear vision for the producer and artist which assures that everyone is on the same page.'
        },
        {
        img: '/EEC4FE20-FF48-4239-B0A6-A73AE8193304.jpg',
        alt: '',
        label: 'The Equipment',
        text: 'For recording vocals we have an acoustically treated vocal booth. The two pieces of equipment you’ll find inside are the Shure SM57 microphone and a pair of Audio Technica ATH M40x’s. All vocals are recorded through the Apollo Twin X DUO audio interface and processed in Ableton Live or Logic Pro. The booth also has LED lights installed with a remote control so that you can choose the color that best suits you while inside. When recording instrumentation we have a standard way of doing things, but we are always open to suggestions and new ideas. If there is a specific way you would like to record something we will accompany that to our best ability.'
        },
        {
        img: '/IMG-4072.jpg',
        alt: '',
        label: 'Integrity Check',
        text: "Before hitting the record button we will check tuning and each microphone's response to instrumentation and voice. In band settings this includes checking the microphones response between the instruments that are opposite of each other. Such as the guitar one’s microphone will record guitar two to check for bleeding then vice versa. Upon that we will capture approximately ten seconds of performance and listen to the playback. That way you have an idea of how everything will sound once fully recorded. Now we are ready to go."
        }
        ]
</script>

<section class="the-process">
    <h2>The Process</h2>
    {#each processes
 as process}
         <div class="process" in:fly={{y: 200, duration: 2000 }}>
			<span class="text">
                <h3 class="label">
                    {process.label}
                </h3>
				{process.text}
			</span>
			<img src={process.img} alt={process.alt} />
		</div>
	{/each}
</section>


<style lang="scss">
	.the-process {
		width: 100%;
		max-width: var(--screen-lg);
        padding: 12rem 2rem 8rem;

        h2 {
            font-size: 4rem;
            margin-bottom: 6rem;
        }
        h3 {
            font-size: 2.5rem;
            margin-bottom: 8rem;
            /* color: #a3998b; */
            color: #e1d7cb;
            /* color: #c3bbb0; */
        }

		.process + .process {
			margin-top: 10rem;
		}

        .process:nth-of-type(odd) {
			grid-template:
				'text img' 600px
				/ 2fr 3fr;
        }

        .process:nth-of-type(even) {
			grid-template:
				'img text' 600px
				/ 3fr 2fr;
        }

		.process {
			display: grid;
			width: 100%;
			align-items: center;
			grid-gap: 4rem;

			@media only screen and (max-width: 1200px) {
				grid-template:
					'text img' 600px
					/ 300px 1fr;
				.number {
					display: none;
				}
			}

            .label {
                font-family: var(--font-secondary);
                font-size: 2.5rem;
                margin-bottom: 0.5rem;
            }

            .sep {
                background: var(--c-sec);
                height: 4px;
                width: auto;
            }

			.text {
				grid-area: text;
                line-height: 1.35;
			}

			.number {
				grid-area: number;
				position: relative;

				span {
					z-index: 1;
					font-size: 16rem;
					position: relative;
					font-family: 'Abril Fatface', serif;
				}

				.number-underline {
					display: none;
					position: absolute;
					transform: translateY(-120px);
					background-color: var(--c-ter);
					height: 8rem;
					width: 100%;
				}
			}

			img {
				grid-area: img;
				width: 100%;
				height: 100%;
				object-fit: cover;
			}
		}

        @media only screen and (max-width: 800px) {
            h2 {
                margin-bottom: 4rem;
            }

            .process + .process {
                margin-top: 6rem;
            }

            .process {
                grid-gap: 2rem;
            }

            .process:nth-of-type(odd) {
                grid-template:
                    'img' 400px
                    'text' auto
                    / 1fr;
            }

            .process:nth-of-type(even) {
                grid-template:
                    'img' 600px
                    'text' auto
                    / 1fr;
            }

        }

        @media (max-width: 550px) {
            padding: 8rem 1rem 4rem;
            .process + .process {
                margin-top: 4rem;
            }
        }
	}

</style>
