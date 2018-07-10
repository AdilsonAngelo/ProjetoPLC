
#include <SDL.h>
#include <SDL_mixer.h>

// Mix_LoadWAV(file) Mix_LoadWAV_RW(SDL_RWFromFile(file, "rb"), 1)

Mix_Chunk * HS_Mix_LoadWAV(const char* file) {
    Mix_LoadWAV(file);
}
