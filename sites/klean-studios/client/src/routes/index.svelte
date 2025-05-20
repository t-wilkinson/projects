<script context="module" lang="ts">
	import type { Load } from '@sveltejs/kit';
	import { getApi, getAttributes } from '$lib/api.js';

	export const load: Load = async () => {
		let [projects, testimonials] = await Promise.all([
			getApi('/projects?populate=*'),
			getApi('/testimonials')
		]);
		projects.data = (projects.data || []).map(({ attributes }) => ({
			name: attributes.name,
			/* description: project.description, */
			image: attributes.image,
			url: attributes.url,
            order: attributes.order,
			/* tags: project.tags.map(tag => ({ */
			/*     id: tag.id.toString(), */
			/*     name: tag.name, */
			/*     color: tag.color */
			/* })) */
		}));

		return {
            status: projects.status,
			props: {
				projects: projects.data.sort((a, b) => b.order - a.order),
				testimonials: getAttributes(testimonials.data)
			}
		};
	};
</script>

<script lang="ts">
    import Welcome from '$lib/components/Welcome.svelte'
	import Projects from '$lib/components/Projects/index.svelte';
	import {
		Hero,
		Services,
		TheSpace,
		TheProcess,
		MasteringComparison,
		Testimonials,
		AboutMax,
		Mission,
		Beats,
		ServiceForm
	} from '$lib/components/Home/index.svelte';

	export let testimonials = [];
	export let projects: Projects[] = [];
</script>

<Welcome />
<Hero />
<Services />
<TheSpace />
<TheProcess />
<MasteringComparison />
<Testimonials {testimonials} />
<section class="projects">
	<h2>Projects</h2>
	<Projects {projects} />
</section>
<Mission />
<AboutMax />
<Beats />
<ServiceForm />

<style lang="scss">
	:global {
		h1 {
			font-size: 8em;
		}
		h2 {
			font-size: 5rem;
		}
		h3 {
			font-size: 2rem;
		}

        @media (max-width: 1000px) {
            h2 {
                font-size: 2rem;
            }
        }
	}

	.projects {
		width: 100%;
		max-width: var(--screen-md);
		margin-bottom: 8rem;
        padding: 0 2rem;

		h2 {
			font-size: 4rem;
		}

        @media (max-width: 600px) {
            margin-bottom: 0rem;
        }

        @media (max-width: 400px) {
            margin-bottom: 4rem;
            h2 {
                font-size: 4rem;
            }
        }
	}
</style>
