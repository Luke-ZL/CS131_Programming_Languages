import asyncio
import aiohttp
import sys
import json
import time
import logging
import re

server_name = sys.argv[1]

server_ports = {
    "Goloman":12264,
    "Hands":12265,
    "Holiday":12266,
    "Welsh":12267,
    "Wilkes":12268,
	}

api_key = "AIzaSyDYKBO7w-kzeei09p-LQgJvXvPXsucQD5E"

server_neighbour = {'Goloman':['Hands','Holiday','Wilkes'],
				   'Hands':['Wilkes', 'Goloman'],
				   'Holiday':['Welsh','Wilkes', 'Goloman'], 
				   'Wilkes':['Goloman','Hands','Holiday'], 
				   'Welsh':['Holiday']
				   }

logging.basicConfig(filename='{}.log'.format(server_name),
                    format='%(asctime)s %(levelname)s %(message)s',
                    level=logging.INFO)

client_list = {}


def is_number(s):
    try:
        float(s)
        return True
    except ValueError:
        return False

def check_validity(kw, pl):
    if kw == 'IAMAT':
        if len(pl) != 3:
            print('invalid IAMAT argument length {}'.format(len(pl)))
            return False
        if (pl[1].count('+') + pl[1].count('-')) != 2:
            print('invalid IAMAT argument')
            return False            
        if pl[1][0] != '+' and pl[1][0] != '-':
            print('invalid IAMAT argument')
            return False
        if not is_number(pl[2]):
            print('invalid IAMAT argument')
            return False
    elif kw =='WHATSAT':
        if len(pl) != 3:
            print('invalid WHATSAT argument length {}'.format(len(pl)))
            return False
        if (not is_number(pl[1])) or (not is_number(pl[2])):
            print('invalid WHATSAT argument')
            return False
        if float(pl[1]) >= 50 or float(pl[1]) <= 0 or float(pl[2]) >= 20 or float(pl[2]) <= 0:
            print('invalid WHATSAT argument')
            return False
        if not pl[0] in client_list:
            print('invalid WHATSAT argument')
            return False
    elif kw == 'AT':
        if len(pl) < 6:
            print('invalid AT argument length {}'.format(len(pl)))
            return False
    return True

async def flood_algo(message):
    ori_msg = message.split()[:6]
    received_list = message.split()[6:]
    for neighbour in server_neighbour:
        if neighbour not in received_list:
            try:
                logging.info('flooding message to server {}'.format(neighbour))
                print('flooding message to server {}'.format(neighbour))
                _, writer = await asyncio.open_connection('127.0.0.1', port = server_ports.get(neighbour), loop=loop)
                writer.write(message.encode())
                await writer.drain()
                writer.close()
                logging.info('Successfully flood message to server {}'.format(neighbour))
                print('Successfully flood message to server {}'.format(neighbour))
            except:
                logging.info('Failed to connect to {}'.format(neighbour))
                print('Failed to connect to {}'.format(neighbour))

        
async def handle_iamat(pl):
    diff = time.time() - float(pl[2])
    if time_diff > 0:
        time_diff = '+' + str(diff)
    else:
        time_diff = str(diff)

    response_msg = 'AT {} {} {} {} {}\n'.format(server_name, time_diff, pl[0], pl[1], pl[2])
    logging.info('reponse sent: {}'.format(response_msg))
    print('reponse sent: {}'.format(response_msg))

    if not (pl[0] in client_list) or float(client_list.get(pl[0]).split()[5]) < pl[2]:
        logging.info('Updating client_list for {}'.format(pl[0]))
        print('Updating client_list for {}'.format(pl[0]))
        client_list[pl[0]] = response_msg
        logging.info('Server {} floods client {} info'.format(server_name, pl[0]))
        print('Server {} floods client {} info'.format(server_name, pl[0]))
        message = 'AT {} {} {} {} {} {}\n'.format(server_name, time_diff, pl[0], pl[1], pl[2], server_name)
        asyncio.create_task(flood_algo(messsage))
    else:
        logging.info('server {} is up-to-date'.format(server_name))
        print('server {} is up-to-date'.format(server_name))
    return response_msg

async def fetch(session, url):
    async with session.get(url) as response:
        return await response.text()

async def handle_whatsat(pl):
    message = client_list.get(pl[0])
    lalo = re.split('[+-]', message.split()[4])
    la = lalo[1]
    lo = lalo[2]
    sym_list = []
    for i in message.split()[4]:
        if i == '+':
            sym_list.append('')
        if i == '-':
            sym_list.append(i)

    loc = '{}{},{}{}'.format(sym_list[0],la,sym_list[1],lo)
    url = 'https://maps.googleapis.com/maps/api/place/nearbysearch/json?location={}&radius={}&key={}'.format(loc,int(pl[1]) * 1000, api_key)
    async with aiohttp.ClientSession() as session:
        res = await fetch(session, url)
		js_res = json.loads(res)
		js_res['results'] = js_res['results'][:int(pl[2])]
		response = json.dumps(js_res, indent=4)
		logging.info('server {} queries Google Place API'.format(server_name))
		print('server {} queries Google Place API'.format(Server))
	return (message+response+'\n\n')

async def handle_input(reader, writer):
    logging.info('Server {} initialized'.format(server_name))
    print('Server {} initialized'.format(server_name))
    data = await reader.readline()
    decoded= data.decode()
    splitted = decoded.splilt()
    keyword = splitted[0]
    payload = splitted[1:]

    if keyword == 'IAMAT' and check_validity(keyword,payload):
        logging.info('Server {} recieves a valid IAMAT message'.format(server_name))
        print('Server {} recieves a valid IAMAT message'.format(server_name))
        msg = await handle_iamat(payload)
    elif keyword == 'WHATSAT' and check_validity(keyword,payload):
        logging.info('Server {} recieves a valid WHATSAT message'.format(server_name))
        print('Server {} recieves a valid WHATSAT message'.format(server_name))
        msg = await handle_whatsat(payload)
    elif keyword == 'AT' and check_validity(keyword,payload):
        logging.info('Server {} recieves a valid AT message'.format(server_name))
        print('Server {} recieves a valid AT message'.format(server_name))
        if not (payload[2] in client_list) or float(client_list.get(payload[2]).split()[5]) < payload[2]:
            logging.info('AT: Updating client_list for {}'.format(payload[2]))
            print('AT: Updating client_list for {}'.format(payload[2]))
            client_list[payload[2]] = splitted[:6]
            logging.info('Server {} floods client {} info'.format(server_name, payload[2]))
            print('Server {} floods client {} info'.format(server_name, payload[2]))
            asyncio.create_task(flood_algo(decoded+' '+server_name))
    else:
        logging.info('Server {} receives an invalid message'.format(server_name))
        print('Server {} receives an invalid message'.format(server_name))
        msg = '? {}\n'.format(decoded)
    logging.info('Writing: {}', msg)
    writer.write(msg.encode())
    await writer.drain()

def main():
	if len(sys.argv) != 2:
		print('Wrong arg num')
		sys.exit(1)
	if sys.argv[1] not in server_neighbour:
		print('Wrong server name')
		sys.exit(1)
	
	global loop
	loop = asyncio.get_event_loop()
	coro = asyncio.start_server(handle_input, '127.0.0.1', port_dic[sys.argv[1]], loop=loop )
	server = loop.run_until_complete(coro)
	
    logging.info('Server starts')
    print('Server starts')

	try:
		loop.run_forever()
	except KeyboardInterrupt:
		pass
	
	server.close()
	loop.run_until_complete(server.wait_closed())
	loop.close()
	
    logging.info('Server closes')
    print('Server closes')

if __name__ == '__main__':
	main()
