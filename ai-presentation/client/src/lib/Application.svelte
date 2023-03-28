<script>
    import Slide from "$lib/Slide.svelte";
    export let title = '';
    export let description = '';
    export let subtitle = "Application";
</script>

<Slide>
    <div class="flex flex-wrap justify-evenly mx-auto max-w-22/24 pt-20">
        <div class="xl:w-11/24 w-22/24">
            <p
                class="font-sans font-bold text-sm text-blue-600 tracking-widest uppercase"
            >
            {subtitle}
            </p>
            <h1
                class="font-sans font-black text-neutral-900 text-5xl text-left mt-8 tracking-tighter"
            >
            {title}
            </h1>
            <p
                class="font-sans font-light text-3xl text-gray-700 mt-8 leading-relaxed"
            >
            {description || ''}
            </p>
        </div>
        <div class="xl:w-11/24 w-22/24">
            <div
                class="flex flex-wrap justify-evenly items-top mx-auto max-w-22/24"
            >
            <slot />
            <!--
                <div
                    class="w-23/24 md:w-11/24 py-10 animate__animated op0 start-1:op100 start-1:animate__fadeInRight"
                />
            -->
            </div>
        </div>
    </div>
</Slide>

