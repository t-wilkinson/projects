<script>
	import "uno.css";

    let SERVER_API = 'http://192.168.0.100:3008'
    let imgSrc = { style: null, content: null };
    let imgFile = { style: null, content: null }
    let imgInputs = { style: null, content: null };
    let outputImg = '';
    let status = null;

    const runStyleTransfer = (e) => {
        if (status === 'submitting') {
            return
        }

        e.preventDefault()
        if (!imgFile.style || !imgFile.content) {
            return
        }
        status = 'submitting'

        const formData = new FormData()
        formData.append('style', imgFile.style)
        formData.append('content', imgFile.content)

        fetch(`${SERVER_API}/style-transfer`, {
            method: "POST",
            body: formData
        })
            .then(res => res.blob())
            .then(imageBlob => {
                outputImg = URL.createObjectURL(imageBlob)
                status = 'success'
            })
            .catch(err => {
                console.error(err)
                status = 'error'
            })
    }

    const onFileSelected = (e) =>{
        const name = e.target.name
        let image = e.target.files[0];
        imgFile[name] = image

        let reader = new FileReader();
        reader.readAsDataURL(image);
        reader.onload = e => {
            imgSrc[name] = e.target.result
        };
    }
</script>

<div id="app">
    <h1>Upload images</h1>
    <form
        on:submit={runStyleTransfer}
    >
        <div class="upload-container">
            <div class="upload">
                {#if imgSrc.style}
                    <img class="avatar" src={imgSrc.style} alt="d" />
                {/if}
                <button type="button" class="image-upload" on:click={()=>{imgInputs.style.click();}}>Style</button>
                <input name="style" style="display:none" type="file" accept=".jpg, .jpeg, .png" on:change={(e)=>onFileSelected(e)} bind:this={imgInputs.style} >
            </div>

            <div class="upload">
                {#if imgSrc.content}
                    <img class="avatar" src={imgSrc.content} alt="d" />
                {/if}
                <button type="button" class="image-upload" on:click={()=>{imgInputs.content.click();}}>Content</button>
                <input name="content" style="display:none" type="file" accept=".jpg, .jpeg, .png" on:change={(e)=>onFileSelected(e)} bind:this={imgInputs.content} >
            </div>
        </div>

        <button type="submit" class="submit">
            Transfer
        </button>
        <span>
            {#if status === 'submitting'}
                submitting...
            {:else if status === 'error'}
                error!
            {/if}
        </span>
    </form>

    <h1 class="result">Result</h1>
    {#if outputImg}
        <img src={outputImg} alt="Output" class="output-image" />
    {/if}
</div>

<style>
    h1 {
        margin: 0;
        margin-bottom: 0.5rem;
        font-size: 1.5rem;
    }

    #app{
        display:flex;
        align-items:center;
        justify-content:center;
        flex-flow:column;
    }

    .result {
        margin-top: 1rem;
    }

    form {
        display: flex;
        flex-direction: column;
        align-items: center;
    }

    .image-upload {
        width: 100%;
    }

    button {
        padding: 1rem 2rem;
        border: none;
        background: #ccc;
        cursor: pointer;
    }

    .submit {
        margin-top: 0.5rem;
        background: rebeccapurple;
        color: white;
    }

    .upload-container {
        display:flex;
    }

    .upload{
        padding: 0.25rem;
    }

    img {
        display:flex;
        height: 100px;
        width: 100%;
        object-fit: cover;
    }

    .output-image {
        width: 300px;
        height: 400px;
        object-fit: contain;
        transform: rotate(90deg) translateX(-50px);
    }
</style>


