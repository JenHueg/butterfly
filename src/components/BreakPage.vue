# BreakPage.vue

<!-- ====================================
TRANSPARENCY
This code was developed using the help of AI. The model used: 
OpenAI.(2026). ChatGPT (GPT -5.6 Luna). [Large language model]. https.://chatgpt.com
OpenAI.(2026). ChatGPT (GPT -6). [Large language model]. https.://chatgpt.com 

PLEASE ADD HERE OTHER MODELS THAT ARE USED WHEN DEVELPING THIS CODE FURTHER: 

    ======================================== -->
    
```vue
<template>
  <div class="page">
    <h1>Pause</h1>

    <p>
      Sie haben den ersten Teil des Experiments abgeschlossen.
    </p>

    <p>
      Machen Sie gerne eine kurze Pause.
    </p>

    <p class="instruction">
      Drücken Sie die Taste <strong>B</strong>, um mit dem zweiten Block fortzufahren.
    </p>

    <p>
      Die Pause endet automatisch in:
    </p>

    <h2>{{ formattedRemainingTime }}</h2>

  </div>
</template>

<script>
export default {
  name: 'BreakPage',

   data() {
    return {
      remainingSeconds: 300,
      breakEndsAt: null,
      timerId: null,
      hasContinued: false
    }
  },  

  mounted() {
    console.log("BreakPage ist geladen")

    // -------------------------------------
    // 5 MINUTE COUNTDOWN
    // -------------------------------------

    this.breakEndsAt =
      Date.now() + 5 * 60 * 1000

    this.updateCountdown()

    this.timerId = window.setInterval(() => {
      this.updateCountdown()
    }, 1000)

    window.addEventListener('keydown', this.handleKeydown)
  },

  beforeDestroy() {

    // countdown stopp 

    if (this.timerId !== null) {
      clearInterval(this.timerId)
    }

    // reomove keyboard listener 

    window.removeEventListener('keydown', this.handleKeydown)
  },

  computed: {

    formattedRemainingTime() {

      const minutes =
        Math.floor(this.remainingSeconds / 60)

      const seconds =
        this.remainingSeconds % 60

      return [
        String(minutes).padStart(2, '0'),
        String(seconds).padStart(2, '0')
      ].join(':')

    }

  },

  methods: {

    // -------------------------------------
    // UPDATE COUNTDOWN
    // ------------------------------------

    updateCountdown() {

      const remainingMs =
        this.breakEndsAt - Date.now()

      this.remainingSeconds =
        Math.max(
          0,
          Math.ceil(remainingMs / 1000)
        )

      // Automatically continue after 5 minutes
      if (remainingMs <= 0) {
        this.continueToNextBlock()
      }

    },

    // ----------------------------------------
    // KEYBOARD SHORTCUT B 
    // ----------------------------------------

   handleKeydown(event) {

      if (
        event.key === 'b' ||
        event.key === 'B'
      ) {
        this.continueToNextBlock()
      }

    },

    // ------------------------------------------
    // CONTINUE TO BLOCK 2
    // ------------------------------------------

    continueToNextBlock() {

      // Prevent multiple transitions
      if (this.hasContinued) {
        return
      }

      this.hasContinued = true

      // Stop countdown
      if (this.timerId !== null) {
        clearInterval(this.timerId)
        this.timerId = null
      }

      // Remove keyboard listener
      window.removeEventListener(
        'keydown',
        this.handleKeydown
      )

      // Notify App.vue
      this.$emit('next')

    }
  }
}
</script>

<style scoped>
.page {
  max-width: 850px;
  margin-top: 30px;
  padding: 12px 28px;
  text-align: left;
  font-family: Arial, sans-serif;
  font-size: 18px;
}

.instruction {
  font-size: 18px;
}
</style>
```
