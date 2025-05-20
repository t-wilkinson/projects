<script lang="ts">

  import { inview } from 'svelte-inview';

  let isInView = false;
  let scrollDirection;

  const handleChange = ({ detail }) => {
    isInView = detail.inView;
    scrollDirection = detail.scrollDirection.vertical;
  };
</script>

<div
    use:inview={{
        rootMargin: '-50px',
        unobserveOnEnter: false,
    }}
    on:change={handleChange}
        class:animate={isInView}
        class="container"
    >
    <slot />
</div>

<style lang="scss">
    .container {
        opacity: 0;
        transform: translateY(50px);
    }

    @keyframes -animate {
        0% {
            opacity: 0;
            transform: translateY(50px);
        }
        100% {
            opacity: 1;
            transform: translateY(0);
        }
    }
    .animate {
        animation: -animate 0.2s ease-in forwards;
    }
</style>
