;; haiku.el --- Semi-random haiku generator

;; Author: Jose E. Marchesi <jemarch@gnu.org>
;; Maintainer: Jose E. Marchesi <jemarch@gnu.org>

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Haiku generator for erbot.

;;; Code:

(setq erbot-haiku-quotes-1 
      '(
	"The street-smart seamstress "
	"The young Russian bride "
	"The substitute nurse "
	"The Polish waitress "
	"The baroness stirs, "
	"The long-legged blonde "
	"The dish-water blonde "
	"The bow-legged midget "
	"The busty brunette "
	"The divorcee sighs, "
	"The arthritic nun "
	"The loquacious nurse "
	"The hip-sprung school marm "
	"The one-eyed baker "
	"The plumber's third wife "
	"Traffic stills. The maid "
	"Clouds form. The pornstar "
	"The stewardess coughs, "
	"The Swiss bank teller "
	"The stripper pauses, "
	"The erstwhile diva "
	"The languid bar maid "
	"The opera singer "
	"The zoologist "
	"The Czech spinster "
	"His virgin great-aunt "
	"His neighbor's young wife "
	"The banker's mistress "
	"The pregnant midwife "
	"The devious moll "
	"The shy farmer's wife "
	"A cornfed she-spy "
	"The juice bar clerk's wife "
	"One hillbilly tart "
	"The one in the skirt "
	"The kiwi au pair "
	"The lipstick model "
	"A lady from Minsk "
	"The gal with Shooter "
	"The slatternly nurse "
	"The B-movie star "
	"The heart-broken girl "
	"The star-struck waitress "
	"The therapist snores, "
	"The stewardess drools, "
	"The magnate's mistress "
	"The steel baron's bride "
	"The pianistas niece "
	"The Russian cellist "
	"The poetess gulps, "
	"The Slavic wet nurse "
	"Filipino Sue "
	"Susannah stretches, "
	"The wet nurse sniffles, "
	"The clarinetist "
	"The drunk southern belle "
	"The cheer squad reject "
	"The home ec teacher "
	"The receptionist "
	"The paralegal "
	"The street-smart fly girl "
	"The redhead stretches, "
	"The old-fashioned nun "
	"The ice cream lady "
	"The sullen milkmaid "
	"The vain meter maid "
	"The fat Dixie Chick "
	"The shy cartoonist "
	"The sexy bassist "
	"The reclusive aunt "
	"The sly lunch lady "
	"The Czech go-go girl "
	"The short cheerleader "
	"The chain-smoking niece "
	"The Swiss governess "
	"The stone-faced matron "
	"The suave landlady "
	"Traffic slows. The nun "
	"Paint dries. The brunette "
	"The other woman "
	"The anchorwoman "
	"The Russian madam "
	"His ex-fiancee "
	"The young blushing bride "
	"The widow-to-be "
	"The drunken bridesmaid "
	"The groom's ex-wife sneers, "
	"The gap-toothed redneck "
	"The night-shift seamstress "))
  
        
(setq erbot-haiku-quotes-2
      '(
	"removes her prosthetic leg. "
	"rolls her tongue, trilling r's, l's. "
	"wakes, deflates the air mattress. "
	"bathes in warm crocodile tears. "
	"motions with her silver thumb. "
	"rouges her razorous cheeks. "
	"arches her wrist towards the sky. "
	"removes her golden fake nose. "
	"stands, coins spilling from her ears. "
	"removes her prosthetic leg. "
	"rolls her tongue, trilling r's, l's. "
	"wakes, deflates the air mattress.  "
	"bathes in warm crocodile tears. "
	"covers her eyes with sack-cloth. "
	"blots her dark lipstick, pauses. "
	"calmly sets fire to her hair. "
	"hangs her slip on the lanyard. "
	"greasens the stubborn crank-shaft. "
	"polishes the good flatware. "
	"whispers the word 'wheelbarrow'. "
	"fingers her silver tongue-stud. "
	"dreams of monkeys, gibbons, apes. "
	"scrubs the tile floor, knees rasping. "
	"speaks in tongues, eyelids twitching. "
	"retires to the powder room. "
	"dips her tongue in peroxide. "
	"displays her elegant gams. "
	"exhales a plume of wood-smoke. "
	"rings the doorbell, rings again. "
	"rubs her feet, closes her eyes. "
	"shuffles a stack of scratch cards. "
	"grins and waves the poking stick. "
	"leans toward the caged man-child. "
	"rises, but won't run or blink. "
	"shouts lies to baldheaded fools. "
	"really needs to get kneaded. "
	"quickly dons her happy pants. "
	"beckons with a pineapple. "
	"brandishes a hair curler. "
	"plants the pill beneath the sheets. "
	"hides the orange behind the stove. "
	"tucks the gem beneath her tongue. "
	"eats the lottery ticket. "
	"fills the sock drawer with mustard. "
	"steals the swear jar, hops a train. "))

(setq erbot-haiku-quotes-3
      '(
	"Boom-shacka-lacka. "
	"Thunderous applause. "
	"Dogs dance like comets. "
	"The sky fills with stars. "
	"Retrograde motion.  "
	"She sells no sea shells. "
	"No room in the inn. "
	"Dishes dry in sinks. "
	"Snow falls in Utah. "
	"Sirens wail, so close. "
	"He takes a breath, breathes. "
	"His heart swells madly. "
	"His ears fill with blood. "
	"There is always time. "
	"All I've got is time. "
	"You know the story. "
	"The drummer skips town. "
	"One more cigarette. "
	"The ocean shivers. "
	"The dormouse quivers. "
	"Rain falls on spring leaves. "
	"Pizza boy blushes. "
	"The cat fiddles on. "
	"A new moon blushes. "
	"Somewhere a dog howls. "
	"Cats rub themselves mad. "
	"Soup boils on the stove. "
	"The stove eye glows red. "
	"The faucet drips, drips. "
	"Cars howl on the street. "
	"A car backfires, roars. "
	"There is never time. "
	"The bouilabaisse chills. "
	"Ganja cornbread bakes. "
	"People smile and cry. "
	"Spacious rooms are filled. "
	"Unseen lackeys stir. "
	"The burnished door shuts. "
	"For the last time, why? "
	"Why didn't you stay? "
	"Why didn't you leave? "
	"Where did the time go? "
	"My tongue betrays me. "
	"My heart betrays me. "
	"The night betrays me. "
	"I miss you. Love, me. "
	"I've got plenty more. "
	"No, it's not like that. "
	"Cry uncle for me. "
	"Take two steps backwards. "
	"Paper beats rock, fool. "
	"Hell, maybe. Who knows. "
	"Hit me one more time. "
	"My dog has no fleas. "
	"Is this all there is? "
	"No more soup for you. "
	"You buy the next round. "
	"A round of applause. "
	"Contestants titter. "
	"He owns no short-shorts. "
	"Pat your head thusly. "
	"Never with these eyes. "
	"Only with these eyes. "
	"We've all gone crazy. "
	"Hank Williams was right. "
	"It's all circular. "
	"Everything is wet. "
	"Boom-shacka-lacka. "
	"Thunderous applause. "
	"Dogs dance like comets. "
	"The sky fills with stars. "
	"Retrograde motion.  "
	"She sells no sea shells. "
	"No room in the inn. "
	"Dishes dry in sinks. "
	"Snow falls in Utah. "
	"Sirens wail, so close. "
	"He takes a breath, breathes. "
	"His heart swells madly. "
	"His ears fill with blood. "
	"There is always time. "
	"All I've got is time. "
	"You know the story. "
	"The drummer skips town. "
	"One more cigarette. "
	"The ocean shivers. "
	"The dormouse quivers. "
	"Rain falls on spring leaves. "
	"Pizza boy blushes. "
	"The cat fiddles on. "
	"A new moon blushes. "
	"Somewhere a dog howls. "
	"Cats rub themselves mad. "
	"Soup boils on the stove. "
	"The stove eye glows red. "
	"The faucet drips, drips. "
	"Cars howl on the street. "
	"A car backfires, roars. "
	"There is never time. "
	"The bouilabaisse chills. "
	"Ganja cornbread bakes. "
	"People smile and cry. "
	"Spacious rooms are filled. "
	"Unseen lackeys stir. "
	"The burnished door shuts. "
	"For the last time, why? "
	"Why didn't you stay? "
	"Why didn't you leave? "
	"Where did the time go? "
	"My tongue betrays me. "
	"My heart betrays me. "
	"The night betrays me. "
	"I miss you. Love, me. "
	"I've got plenty more. "
	"No, it's not like that. "
	"Cry uncle for me. "
	"Take two steps backwards. "
	"Paper beats rock, fool. "
	"Hell, maybe. Who knows. "
	"Hit me one more time. "
	"My dog has no fleas. "
	"Is this all there is? "
	"No more soup for you. "
	"You buy the next round. "
	"A round of applause. "
	"Contestants titter. "
	"He owns no short-shorts. "
	"Pat your head thusly. "
	"Never with these eyes. "
	"Only with these eyes. "
	"We've all gone crazy. "
	"Hank Williams was right. "
	"It's all circular. "))

(defun fs-haiku (&rest args)
  "REST: args"
  (format "%s\n%s\n%s"
	  (erbutils-random erbot-haiku-quotes-1)
	  (erbutils-random erbot-haiku-quotes-2)
	  (erbutils-random erbot-haiku-quotes-3)))