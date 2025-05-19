<script lang="ts">
	export let testimonials = [];
	let selectedTestimonial = null;

	$: {
		if (testimonials.length > 0 && selectedTestimonial === null) {
			selectedTestimonial = 0;
		}
	}
</script>

<section class="testimonials">
	<div class="testimonials-wrapper">
		<button
      class="left"
			on:click={() => {
				selectedTestimonial -= 1;
				if (selectedTestimonial < 0) {
					selectedTestimonial = testimonials.length - 1;
				}
			}}
		>
			<svg
				viewBox="0 0 34.075 34.075"
			>
				<path
					d="M24.57,34.075c-0.505,0-1.011-0.191-1.396-0.577L8.11,18.432c-0.771-0.771-0.771-2.019,0-2.79
                    L23.174,0.578c0.771-0.771,2.02-0.771,2.791,0s0.771,2.02,0,2.79l-13.67,13.669l13.67,13.669c0.771,0.771,0.771,2.021,0,2.792
                    C25.58,33.883,25.075,34.075,24.57,34.075z"
				/>
			</svg>
		</button>

		{#if testimonials[selectedTestimonial]}
			<div class="testimonial">
				<q class="testimony">{testimonials[selectedTestimonial].testimony}</q>
				<span class="name">{testimonials[selectedTestimonial].name}</span>
				<span class="info">{testimonials[selectedTestimonial].info}</span>
			</div>
		{/if}

		<button
             class="right"
			on:click={() => {
				selectedTestimonial += 1;
				if (selectedTestimonial > testimonials.length - 1) {
					selectedTestimonial = 0;
				}
			}}
		>
			<svg
				viewBox="0 0 34.075 34.075"
                class="right"
			>
            <path
                d="M24.57,34.075c-0.505,0-1.011-0.191-1.396-0.577L8.11,18.432c-0.771-0.771-0.771-2.019,0-2.79
                L23.174,0.578c0.771-0.771,2.02-0.771,2.791,0s0.771,2.02,0,2.79l-13.67,13.669l13.67,13.669c0.771,0.771,0.771,2.021,0,2.792
                C25.58,33.883,25.075,34.075,24.57,34.075z"
                />
			</svg>
		</button>
	</div>
</section>

<style lang="scss">
	.testimonials {
		margin: 0;
		width: 100%;
		display: flex;
		justify-content: center;
		align-items: center;
        margin-bottom: 6rem;

		background: #edf0f9;
        color: #001758;

		.testimonials-wrapper {
			width: 100%;
			max-width: var(--screen-md);
			display: grid;
            grid-template:
                "left testimonial right" auto
                / auto 1fr auto;
            place-items: center;
		}

		button {
			margin: 0 2rem;
			background: none;
			border: none;
			cursor: pointer;
            color: inherit;

            svg {
                width: 1.5rem;
                height: 1.5rem;
                fill: currentColor;
            }
		}

        .left {
            grid-area: left;
        }

        .right {
            grid-area: right;

            svg {
              transform-box: fill-box;
              transform-origin: center;
              transform: rotate(180deg);
            }
        }

		.testimonial {
            grid-area: testimonial;
			height: 25rem;
			padding: 4rem 0;
			display: flex;
			flex-direction: column;
			text-align: center;
			justify-content: center;

            .quote {
                display: inline-block;
                border-radius: 999rem;
                background: var(--c-bg);
                width: 1.5rem;
                height: 1.5rem;
                color: white;
                display: grid;
                place-items: center;
            }

			.testimony {
				font-size: 1.5rem;
                /* color: #7e3b83; */
			}

			.name {
				margin-top: 1em;
				font-size: 1rem;
                /* color: var(--c-pri); */
				font-family: var(--font-secondary);
			}
			.info {
				margin-top: 0.5rem;
				font-family: var(--font-secondary);
			}
		}
	}

    @media (max-width: 600px) {
        .testimonials {
            .testimonials-wrapper {
                padding: 1rem;
                grid-template:
                    "testimonial testimonial" auto
                    "left right" auto
                    / 1fr 1fr;

            }

            button {
                margin: 0;
            }

            .left {
                justify-self: start;
            }

            .right {
                justify-self: end;
            }

            .testimonial {
                height: 20rem;

                .testimony {
                    font-size: 1rem;
                }
            }
        }
    }

</style>
