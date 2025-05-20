<script lang="ts">
	import Footer from '$lib/components/Footer.svelte';
</script>

<!--
<header>
    <a href="#mission">
        Mission
    </a>
    <a href="#about-me">
        About Me
    </a>
    <a href="#services">
        Services
    </a>
    <a href="#the-space">
        The Space
    </a>
    <a href="#testimonials">
        Testimonials
    </a>
    <a href="#projects">
        Projects
    </a>
    <a href="#the-process">
        The Process
    </a>
    <a href="#beats">
        Beats
    </a>
    <a href="#service">
        Create a service
    </a>
</header>
-->
<main>
	<slot />
</main>
<Footer />

<style global lang="scss">
	html,
	body {
		width: 100%;
	}

    header {
        padding: 1rem;
        background: #1e1f22c0;
        z-index: 1;
        position: absolute;
        display: flex;
        top: 0;
        width: 100%;
        justify-content: end;

        a {
            color: white;
            text-decoration: none;
        }
    }

	body {
		margin: 0;
		padding: 0;
		--c-bg: #1e1f22;
		--c-gray: #494f60;
		--c-pri: #b71ac1;
		--c-sec: #f5ff3b;
		--c-ter: #f80000;

		--screen-sm: 600px;
		--screen-md: 800px;
		--screen-lg: 1200px;
		--font-secondary: 'Abril Fatface', serif;
		--font-primary: 'Work Sans', sans-serif;
	}

	* {
		font-family: 'Work Sans', sans-serif;
		box-sizing: border-box;
	}

	a {
		font-family: 'Abril Fatface', serif;
	}

	h1,
	h2,
	h3,
	h4,
	h5,
	h6 {
		margin: 0;
		font-family: 'Abril Fatface', serif;
		font-weight: 400;
	}

	main {
		display: flex;
		flex-direction: column;
		align-items: center;
		justify-content: center;
		width: 100%;
		background: #1e1f22;
		color: white;
		padding: 0;
		margin: 0;
	}
</style>
