<script lang="ts">
	import { toBackendUrl } from '$lib/api.js';
	import { spring } from 'svelte/motion';

	export let projects = [];

    const clamp = (min, max, x) => Math.max(min, Math.min(max, x))
	let hoveredProject = undefined;
	let filter = spring(
		{
			brightness: 1,
			blur: 0
		},
		{
			stiffness: 0.08,
			damping: 0.33
		}
	);

	let coords = spring(
		{
			x: 0,
			y: 0,
			rotation: 0
		},
		{
			stiffness: 0.02,
			damping: 0.55
		}
	);
</script>

<div class="projects">
	{#each projects as project, i}
        {#if i !== 0}
			<div class="separator" />
        {/if}
        <a target="_blank" href={project.url}>
		<div class="project-wrapper">
			{#if hoveredProject === i}
				<img
					class="image"
					style="
                      -webkit-transform:
                        translate3d({$coords.x}px, {$coords.y}px, 0)
                        rotate({$coords.rotation}deg);
                      transform:
                        translate3d({$coords.x}px, {$coords.y}px, 0)
                        rotate({$coords.rotation}deg);
                      filter:
                      blur({$filter.blur}px)
                      brightness({$filter.brightness});
                    "
					src={toBackendUrl(project.image.data.attributes.url)}
					alt={project.name}
				/>
			{/if}

			<div
				class="project"
                on:mouseenter={(e) => {
                    hoveredProject = i
					const bounds = e.currentTarget.getBoundingClientRect();
					coords.set({
						rotation: 0,
						x: e.clientX - bounds.left,
						y: e.clientY - bounds.top
                    }, { hard: true });

                    filter.set({ brightness: 1, blur: 0}, { hard: true })
                }}
				on:mouseleave={() => (hoveredProject = undefined)}
				on:mousemove={(e) => {
					let brightness = 1 + clamp(0, 50, Math.abs(e.movementX) + Math.abs(e.movementY)) * 0.02;
					filter.set(
						{
							brightness: brightness,
							blur: brightness ** 2
						},
						{ soft: 0.1 }
					);

					const bounds = e.currentTarget.getBoundingClientRect();
					coords.set({
						rotation: (e.movementX + e.movementY),
						x: e.clientX - bounds.left,
						y: e.clientY - bounds.top
    }, { soft: 0.1 });

					setTimeout(() => {
						coords.update(({ x, y }) => ({
							x,
							y,
							rotation: 0
						}));
						filter.set({
							brightness: 1,
							blur: 0
						});
					}, 200);
				}}
			>
				<h2 class="name">{project.name}</h2>
			</div>
		</div>
        </a>
	{/each}
</div>

<style lang="scss">
	.projects {
		width: 100%;
	}

	.separator {
		height: 2px;
		background-color: #e0e0e0;
		width: 100%;
		z-index: 1;
		position: relative;
        margin: 1rem 0;
	}

    a {
        color: white;
        text-decoration: none;
    }

	.project-wrapper {
		position: relative;

		.image {
			--width: 32rem;
			--height: 24rem;
			position: absolute;
			width: var(--width);
			height: var(--height);
			left: calc(-10rem);
			top: calc(-10rem);
            object-fit: contain;
		}

		.project {
            text-align: center;
			position: relative;
			padding: 6rem 3rem;
			z-index: 1;
            cursor: pointer;

			.name {
				font-size: 2rem;
			}

            @media (max-width: 400px) {
                .name {
                    font-size: 1.5rem;
                }
            }
		}
	}
</style>
